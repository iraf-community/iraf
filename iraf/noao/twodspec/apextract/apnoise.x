include	<gset.h>
include	<pkg/gtools.h>
include	"apertures.h"


# AP_NOISE -- Model residuals.

procedure ap_noise (ap, gain, dbuf, nc, nl, c1, l1, sbuf, spec, profile, nx, ny,
	xs, ys, sum2, sum4, nsum, nbin, dmin, dmax)

pointer	ap			# Aperture structure
real	gain			# Gain
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
pointer	sbuf			# Sky buffer (NULL if no sky)
real	spec[ny]		# Normalization spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
real	sum2[nbin]		# Sum of residuals squared in bin
real	sum4[nbin]		# Sum of residuals squared in bin
int	nsum[nbin]		# Number of values in bin
int	nbin			# Number of bins
real	dmin, dmax		# Data limits of bins

int	i, ix, iy, ix1, ix2
real	dstep, low, high, s, x1, x2, model, data, ap_cveval()
pointer	cv, sptr, dptr

begin
	dstep = (dmax - dmin) / nbin

	i = AP_AXIS(ap)
	low = AP_CEN(ap,i) + AP_LOW(ap,i)
	high = AP_CEN(ap,i) + AP_HIGH(ap,i)
	cv = AP_CV(ap)

	do iy = 1, ny {
	    i = iy + ys - 1
	    s = ap_cveval (cv, real (i))
	    x1 = max (0.5, low + s) 
	    x2 = min (c1 + nc - 0.49, high + s) 
	    if (x1 > x2)
		next

	    ix1 = nint (x1) - xs[iy] + 1
	    ix2 = nint (x2) - xs[iy] + 1

	    s = spec[iy]
	    if (sbuf != NULL)
		sptr = sbuf + (iy - 1) * nx - 1
	    dptr = dbuf + (i - l1) * nc + nint(x1) - c1
	    do ix = ix1, ix2 {
		if (sbuf != NULL) {
		    model = (s * profile[iy,ix] + Memr[sptr]) / gain
		    sptr = sptr + 1
		} else
		    model = (s * profile[iy,ix]) / gain
		data = Memr[dptr] / gain
		dptr = dptr + 1

		if (model < dmin || model >= dmax)
		    next
		i = (model - dmin) / dstep + 1
		sum2[i] = sum2[i] + (data - model) ** 2
		sum4[i] = sum4[i] + (data - model) ** 4
		nsum[i] = nsum[i] + 1
	    }
	}
end


define	HELP	"noao$twodspec/apextract/apnoise.key"
define	PROMPT	"apextract options"

# AP_NPLOT -- Plot and examine noise characteristics.

procedure ap_nplot (image, im, sigma, sigerr, npts, dmin, dmax)

char	image[SZ_FNAME]		# Image
pointer	im			# Image pointer
real	sigma[npts]		# Sigma values
real	sigerr[npts]		# Sigma errors
int	npts			# Number of sigma values
real	dmin, dmax		# Data min and max

real	rdnoise			# Read noise
real	gain			# Gain

int	i, newgraph, wcs, key
real	wx, wy, x, x1, x2, dx, y, ymin, ymax
pointer	sp, cmd, gp, gt

int	gt_gcur()
real	apgimr()
#int	apgwrd()
#bool	ap_answer()
pointer	gt_init()
errchk	ap_gopen

begin
	# Query user.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	#call sprintf (Memc[cmd], SZ_LINE, "Edit apertures for %s?")
    	#    call pargstr (image)
	#if (!ap_answer ("ansedit", Memc[cmd])) {
	#    call sfree (sp)
	#    return
	#}

	gain = apgimr ("gain", im)
	rdnoise = apgimr ("readnoise", im)

	dx = (dmax - dmin) / npts
	x1 = dmin + dx / 2
	x2 = dmax - dx / 2
	ymin = sigma[1] - sigerr[1]
	ymax = sigma[1] + sigerr[1]
	do i = 2, npts {
	    ymin = min (ymin, sigma[i] - sigerr[i])
	    ymax = max (ymax, sigma[i] + sigerr[i])
	}

	# Set up the graphics.
	call sprintf (Memc[cmd], SZ_LINE, "Noise characteristics of image %s")
	    call pargstr (image)
	call ap_gopen (gp)
	gt = gt_init()
	call gt_sets (gt, GTTITLE, Memc[cmd])
	call gt_sets (gt, GTXLABEL, "Data value")
	call gt_sets (gt, GTYLABEL, "Sigma")
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, "plus")

	# Enter cursor loop.
	key = 'r'
	repeat {
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # Colon commands.
		if (Memc[cmd] == '/')
		    call gt_colon (Memc[cmd], gp, gt, newgraph)
		else
		    call ap_ncolon (Memc[cmd], rdnoise, gain, newgraph)

	    case 'q':
		break

	    case 'r': # Redraw the graph.
		newgraph = YES

	    case 'w': # Window graph
		call gt_window (gt, gp, "gcur", newgraph)

	    case 'I': # Interrupt
		call fatal (0, "Interrupt")

	    default: # Ring bell for unrecognized commands.
		call printf ("\007")
	    }

	    # Update the graph if needed.
	    if (newgraph == YES) {
		call sprintf (Memc[cmd], SZ_LINE,
		    "Read noise = %g e-, Gain = %g e-/DN")
		    call pargr (rdnoise)
		    call pargr (gain)
		call gt_sets (gt, GTPARAMS, Memc[cmd])

		call gclear (gp)
		y = sqrt ((rdnoise/gain)**2 + dmax/gain)
		call gswind (gp, dmin, dmax, ymin, max (ymax, y))
		call gt_swind (gp, gt)
		call gt_labax (gp, gt)
		do i = 1, npts {
		    if (sigma[i] > 0) {
			x = x1 + (i-1) * dx
			call gmark (gp, x, sigma[i], GM_VEBAR+GM_HLINE, -dx/2,
			    -sigerr[i])
		    }
		}
		do i = 1, npts {
		    x = x1 + (i-1) * dx
		    y = sqrt ((rdnoise/gain)**2 + x/gain)
		    if (i == 1)
			call gamove (gp, x, y)
		    else
			call gadraw (gp, x, y)
		}
	        newgraph = NO
	    }

	} until (gt_gcur ("gcur", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)

	# Free memory.
	call gt_free (gt)
	call sfree (sp)
end


# List of colon commands.
define	CMDS		 "|readnoise|gain|"
define	RDNOISE		1	# Read noise
define	GAIN		2	# Gain

# AP_NCOLON -- Respond to colon command from ap_nplot.

procedure ap_ncolon (command, rdnoise, gain, newgraph)

char	command[ARB]		# Colon command
real	rdnoise			# Readout noise
real	gain			# Gain
int	newgraph		# New graph?

real	rval
int	ncmd, nscan(), strdic()
pointer	sp, cmd

begin
	# Scan the command string and get the first word.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (command)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case RDNOISE:
	    call gargr (rval)
	    if (nscan() == 2) {
		rdnoise = rval
		newgraph = YES
	    } else {
		call printf ("rdnoise %g\n")
		    call pargr (rdnoise)
	    }
	case GAIN:
	    call gargr (rval)
	    if (nscan() == 2) {
		gain = rval
		newgraph = YES
	    } else {
		call printf ("gain %g\n")
		    call pargr (gain)
	    }
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}

	call sfree (sp)
end

include	<error.h>
include	<gset.h>
include	<mach.h>
include	"sensfunc.h"

define	KEY		"noao$onedspec/sensfunc/sensfunc.key"
define	PROMPT		"sensfunc options"


# SF_SENSFUNC -- Interactive sensitivity function determination.

procedure sf_sensfunc (gp, stds, nstds, wextn, extn, nextn, sensimage, logfile,
    ecv, function, order, ignoreaps, interactive)

pointer	gp			# Graphics structure
pointer	stds[nstds]		# Pointer to standard observations
int	nstds			# Number of standards
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of extinction table values
char	sensimage[ARB]		# Output rootname
char	logfile[ARB]		# Statistics filename
pointer	ecv			# Residual extinction curve
char	function[ARB]		# Fitting function type
int	order			# Function order
bool	ignoreaps		# Ignore apertures?
int	interactive		# Interactive?

char	cmd[SZ_FNAME]
int	wc, key, newgraph, newfit
real	wx, wy

int	i, j, aperture, shift, npts, fd, open()
real	xmin, xmax, rms, delta
pointer	cv

int	clgcur(), scan(), nscan(), clgwrd()
errchk	open

define	output_	99

begin
	# Initialize data and do the initial fit.
	call sf_reset (stds, nstds, wextn, extn, nextn, ecv, shift)

	xmin = MAX_REAL
	xmax = -MAX_REAL
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    aperture = STD_BEAM(stds[i])
	    xmin = min (xmin, STD_WSTART(stds[i]), STD_WEND(stds[i]))
	    xmax = max (xmax, STD_WSTART(stds[i]), STD_WEND(stds[i]))
	}
	cv = NULL
	call sf_fit (stds, nstds, cv, function, order, xmin, xmax)
	call sf_rms (stds, nstds, rms, npts)

	# If not interactive go to the output.
	if (interactive == 3)
	    goto output_
	if (interactive != 4) {
	    call printf ("Fit aperture %d interactively? ")
		call pargi (aperture)
	    interactive = clgwrd ("answer", cmd, SZ_FNAME, "|no|yes|NO|YES")
	    switch (interactive) {
	    case 1:
		goto output_
	    case 3:
		call sf_gfree (gp)
		goto output_
	    }
	}

	# Initialize graphics structure parameters: airmass and wavelength
	# limits and default images to plot.

	if (gp == NULL)
	    call sf_ginit (gp)
	GP_AIRMASS(gp,1) = MAX_REAL
	GP_AIRMASS(gp,2) = -MAX_REAL
	j = 0
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    GP_AIRMASS(gp,1) = min (GP_AIRMASS(gp,1), STD_AIRMASS(stds[i]))
	    GP_AIRMASS(gp,2) = max (GP_AIRMASS(gp,2), STD_AIRMASS(stds[i]))
	    if (j < SF_NGRAPHS) {
		j = j + 1
	        call strcpy (STD_IMAGE(stds[i]), Memc[GP_IMAGES(gp,j)],
		    SZ_FNAME)
	        call strcpy (STD_SKY(stds[i]), Memc[GP_SKYS(gp,j)], SZ_FNAME)
	    }
	}
	delta = GP_AIRMASS(gp,2) - GP_AIRMASS(gp,1)
	GP_AIRMASS(gp,1) = GP_AIRMASS(gp,1) - 0.05 * delta
	GP_AIRMASS(gp,2) = GP_AIRMASS(gp,2) + 0.05 * delta
	GP_WSTART(gp) = xmin
	GP_WEND(gp) = xmax
	call sf_title (gp, aperture, function, order, npts, rms)

	# Enter cursor loop by drawing the graphs.
	key = 'r'
	repeat {
	    switch (key) {
	    case '?':
		call gpagefile (GP_GIO(gp), KEY, PROMPT)
	    case ':':
		call sf_colon (cmd, gp, stds, nstds, cv, wextn, extn, nextn,
		    ecv, function, order, npts, rms, newfit, newgraph)
	    case 'a':
		call sf_add (gp, stds, nstds, cv, wx, wy, wc)
	    case 'c':
		call sf_composite (stds, nstds, cv)
		newfit = YES
		newgraph = YES
	    case 'd':
		call sf_data (stds, nstds, GP_GRAPHS(gp,wc))
		call sf_nearest (gp, stds, nstds, wx, wy, wc, 0, i, j)
		if (i > 0) {
		    call printf (
		        "%s - Delete p(oint), s(tar), or w(avelength):")
		        call pargstr (STD_IMAGE(stds[i]))
		    if (clgcur ("cursor", wx, wy, wc, key, cmd, SZ_FNAME)==EOF)
		        break
		    call printf ("\n")
		    call sf_delete (gp, stds, nstds, key, i, j)
		}
	    case 'e':
		call sf_extinct (gp, stds, nstds, cv, ecv, function, order)
		newfit = YES
		newgraph = YES
	    case 'f':
		newfit = YES
	    case 'g':
		newgraph = YES
		newfit = YES
	    case 'i':
		call sf_data (stds, nstds, GP_GRAPHS(gp,wc))
		call sf_nearest (gp, stds, nstds, wx, wy, wc, 2, i, j)
		if (i > 0) {
		    call printf (
	    "%s: airmass=%6.3f wavelen=%6.3f sens=%6.3f fit=%6.3f weight=%3f")
		        call pargstr (STD_IMAGE(stds[i]))
			call pargr (STD_AIRMASS(stds[i]))
		        call pargr (Memr[STD_WAVES(stds[i])+j-1])
		        call pargr (Memr[STD_SENS(stds[i])+j-1])
		        call pargr (Memr[STD_FIT(stds[i])+j-1])
		        call pargr (Memr[STD_WTS(stds[i])+j-1])
		}
	    case 'm':
		call sf_data (stds, nstds, GP_GRAPHS(gp,wc))
		call sf_nearest (gp, stds, nstds, wx, wy, wc, 2, i, j)
		if (i > 0) {
		    call printf (
		   "%s - Move p(oint), s(tar), or w(avelength) to cursor:")
		        call pargstr (STD_IMAGE(stds[i]))
		    if (clgcur ("cursor", wx, wy, wc, key, cmd, SZ_FNAME)==EOF)
		        break
		    call printf ("\n")
		    delta = wy - Memr[STD_Y(stds[i])+j-1]
		    call sf_move (gp, stds, nstds, key, i, j, delta)
		}
	    case 'o':
		call sf_reset (stds, nstds, wextn, extn, nextn, ecv, shift)
		newfit = YES
		newgraph = YES
	    case 'q':
		break
	    case 'I':
		call fatal (0, "Interrupt")
	    case 'r':
		newgraph = YES
	    case 's':
		call sf_shift (stds, nstds, shift)
		newfit=YES
		newgraph=YES
	    case 'u':
		call sf_data (stds, nstds, GP_GRAPHS(gp,wc))
		call sf_nearest (gp, stds, nstds, wx, wy, wc, 1, i, j)
		if (i > 0) {
		    call printf (
		        "%s - Undelete p(oint), s(tar), or w(avelength):")
		        call pargstr (STD_IMAGE(stds[i]))
		    if (clgcur ("cursor", wx, wy, wc, key, cmd, SZ_FNAME)==EOF)
		        break
		    call printf ("\n")
		    call sf_undelete (gp, stds, nstds, key, i, j)
		}
	    case 'w':
		call sf_data (stds, nstds, GP_GRAPHS(gp,wc))
		call sf_nearest (gp, stds, nstds, wx, wy, wc, 0, i, j)
		if (i > 0) {
		    call printf (
		        "%s - Reweight p(oint), s(tar), or w(avelength):")
		        call pargstr (STD_IMAGE(stds[i]))
		    if (clgcur ("cursor", wx, wy, wc, key, cmd, SZ_FNAME)==EOF)
		        break
		    call printf ("New weight (%g):")
			call pargr (Memr[STD_IWTS(stds[i])+j-1])
		    call flush (STDOUT)
		    if (scan() != EOF) {
			call gargr (delta)
			if (nscan() == 1)
		    	    call sf_weights (stds, nstds, key, i, j, delta)
		    }
		    call printf ("\n")
		}
	    default:
		call printf ("\007")
	    }

	    # Do a new fit and recompute the RMS, and title string.
	    if (newfit == YES) {
		call sf_fit (stds, nstds, cv, function, order, xmin, xmax)
		call sf_rms (stds, nstds, rms, npts)
		call sf_title (gp, aperture, function, order, npts, rms)
		do i = 1, SF_NGRAPHS
		    if (GP_SHDR(gp,i) != NULL)
			call shdr_close (GP_SHDR(gp,i))
	    }

	    # Draw new graphs.
	    if (newgraph == YES) {
		call sf_graph (gp, stds, nstds, cv, wextn, extn, nextn, ecv)
		newgraph = NO
		newfit = YES
	    }

	    # Overplot new fit.
	    if (newfit == YES) {
		call sf_fitgraph (gp, cv)
		newfit = NO
	    }
	} until (clgcur ("cursor", wx, wy, wc, key, cmd, SZ_FNAME) == EOF)

	# Close any open images.
	do i = 1, SF_NGRAPHS
	    if (GP_SHDR(gp,i) != NULL)
	        call shdr_close (GP_SHDR(gp,i))

output_
	# Output the sensitivity function and logfile statistics.
	call sf_output (stds, nstds, cv, sensimage, ignoreaps)
	if (logfile[1] != EOS) {
	    iferr {
	        fd = open (logfile, APPEND, TEXT_FILE)
	        call sf_stats (fd, stds, nstds, function, order, npts, rms)
	        call sf_vstats (fd, stds, nstds, cv, wextn, extn, nextn, ecv)
	        call close (fd)
	    } then
		call erract (EA_WARN)
	}
	call cvfree (cv)
end

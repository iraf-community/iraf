include	<error.h>
include	<pkg/gtools.h>

define	HELP	"noao$onedspec/ecidentify/ecffit/ecffit.key"
define	PROMPT	"fitcoords surface fitting options"

# EC_FIT -- Echelle dispersion fitting.
#
#	X - Pixel coordinates along dispersion
#	Y - Relative order number
#	Z - Wavelength

procedure ecf_fit (ecf, gp, gt, xd, yd, zd, wd, npts, fixedorder)

pointer	ecf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
double	xd[npts]		# Pixel coordinates along dispersion
double	yd[npts]		# Order number
double	zd[npts]		# Wavelength
double	wd[npts]		# Weights
int	npts			# Number of points
int	fixedorder		# Fixed order?

real	wx, wy
int	wcs, key
int	i, newgraph
pointer	sp, wd1, rd, xr, yr
char	cmd[SZ_LINE]

int	ecf_nearest()
int	clgcur(), scan(), nscan()
errchk	ecf_solve()
include	"ecffit.com"

begin
	# Allocate residuals and weights with rejected points arrays
	call smark (sp)
	call salloc (wd1, npts, TY_DOUBLE)
	call salloc (rd, npts, TY_DOUBLE)
	call amovd (wd, Memd[wd1], npts)

	# Compute a solution and return if not interactive.
	if (gp == NULL) {
	    call ecf_solve (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
		fixedorder)
	    call ecf_reject (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
		fixedorder)
	    do i = 1, npts
		if (Memd[wd1+i-1] != wd[i])
		    wd[i] = -1.
	    call sfree (sp)
	    return
	}

	# Allocate real graph vectors.
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)

	# Read cursor commands.
	key = 'f'
	repeat {
	    switch (key) {
	    case 'o':
		call printf ("Order offset (%d): ")
		    call pargi (offset)
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargi (i)
		    if (nscan() == 1)
			offset = i
		    call amovd (wd, Memd[wd1], npts)
		    call ecf_solve (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
			YES)
		    call ecf_reject (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
			YES)
		    call ecf_gdata (ecf, xtype, xd, yd, zd, Memd[rd],
			Memr[xr], npts)
		    call ecf_gdata (ecf, ytype, xd, yd, zd, Memd[rd],
			Memr[yr], npts)
		    call ecf_title (gt)
		    newgraph = YES
		}

	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # List or set parameters
		if (cmd[1] == '/')
		    call gt_colon (cmd, gp, gt, newgraph)
		else
		    call ecf_colon (cmd, gp)

	    case 'x': # Set ordinate
		call printf ("Ordinate - ")
		call printf (
		    "(p)ixel, (o)rder, (w)avelength, (r)esidual, (v)elocity: ")
		if (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    break

		if (key != xtype) {
		    if (key=='p'||key=='o'||key=='w'||key=='r'||key=='v') {
			xtype = key
		        call gt_setr (gt, GTXMIN, INDEF)
		        call gt_setr (gt, GTXMAX, INDEF)
		        call ecf_gdata (ecf, xtype, xd, yd, zd, Memd[rd],
			    Memr[xr], npts)
		        call ecf_title (gt)
			newgraph = YES
		    } else
			call printf ("\007")
		}

	    case 'y': # Set abscissa
		call printf ("Abscissa - ")
		call printf (
		    "(p)ixel, (o)rder, (w)avelength, (r)esidual, (v)elocity: ")
		if(clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
		    break

		if (key != ytype) {
		    if (key=='p'||key=='o'||key=='w'||key=='r'||key=='v') {
			ytype = key
		        call gt_setr (gt, GTYMIN, INDEF)
		        call gt_setr (gt, GTYMAX, INDEF)
		        call ecf_gdata (ecf, ytype, xd, yd, zd, Memd[rd],
			    Memr[yr], npts)
		        call ecf_title (gt)
			newgraph = YES
		    } else
			call printf ("\007")
		}

	    case 'r': # Redraw
		newgraph = YES

	    case 'c': # Cursor coordinates
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    wd, npts)
		call printf ("%10.2g %d %10.8g\n")
		    call pargd (xd[i])
		    call pargd (yd[i])
		    call pargd (zd[i])

	    case 'd': # Delete
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    wd, npts)
		if (i > 0)
		    Memd[wd1+i-1] = wd[i]

	    case 'u': # Undelete
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    wd, npts)
		if (i > 0)
		    Memd[wd1+i-1] = wd[i]

	    case 'f': # Fit
		call amovd (wd, Memd[wd1], npts)
		call ecf_solve (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
		    fixedorder)
		call ecf_reject (ecf, xd, yd, zd, Memd[wd1], Memd[rd], npts,
		    fixedorder)
		call ecf_gdata (ecf, xtype, xd, yd, zd, Memd[rd],
		    Memr[xr], npts)
		call ecf_gdata (ecf, ytype, xd, yd, zd, Memd[rd],
		    Memr[yr], npts)
		call ecf_title (gt)
		newgraph = YES

	    case 'w': # Window graph
		call gt_window (gt, gp, "cursor", newgraph)

	    case 'q': # Quit
		break

	    case 'I': # Interrupt
		call fatal (0, "Interrupt")

	    default: # Ring the bell.
		call printf ("\07\n")
	    }

	    if (newgraph == YES) {
		call ecf_graph (gp, gt, Memr[xr], Memr[yr], wd, Memd[wd1], npts)
		newgraph = NO
	    }
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	do i = 1, npts
	    if (Memd[wd1+i-1] != wd[i])
		wd[i] = -1.
	call sfree (sp)
end

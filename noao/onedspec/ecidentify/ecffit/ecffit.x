include	<error.h>
include	<pkg/gtools.h>

define	HELP	"noao$lib/scr/ecffit.key"
define	PROMPT	"fitcoords surface fitting options"

# EC_FIT -- Echelle dispersion fitting.
#
#	X - Pixel coordinates along dispersion
#	Y - Relative order number
#	Z - Wavelength

procedure ecf_fit (ecf, gp, gt, xd, yd, zd, wd, npts)

pointer	ecf			# GSURFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
double	xd[npts]		# Pixel coordinates along dispersion
double	yd[npts]		# Order number
double	zd[npts]		# Wavelength
double	wd[npts]		# Weights
int	npts			# Number of points

real	wx, wy
int	wcs, key
int	i, newgraph
pointer	sp, xr, yr, wr
char	cmd[SZ_LINE]

int	ecf_nearest()
int	clgcur(), scan(), nscan()
errchk	ecf_solve(), ecf_solve1()
include	"ecffit.com"

begin
	# Compute a solution and return if not interactive.
	if (gp == NULL) {
	    call ecf_solve (ecf, xd, yd, zd, wd, npts)
	    return
	}

	# Allocate real graph vectors.
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (wr, npts, TY_REAL)
	call achtdr (wd, Memr[wr], npts)

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
		    call printf ("Fitting with order offset %d ...")
			call pargi (offset)
		    call flush (STDOUT)
		    call ecf_solve1 (ecf, xd, yd, zd, wd, npts)
		    call ecf_gdata (ecf, xtype, xd, yd, zd, Memr[xr], npts)
		    call ecf_gdata (ecf, ytype, xd, yd, zd, Memr[yr], npts)
		    call ecf_title (gt)
		    newgraph = YES
		}

	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # List or set parameters
		if (cmd[1] == '/')
		    call gt_colon (cmd, gp, gt, newgraph)
		else
		    call ecf_colon (cmd, gp, ecf)

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
		        call ecf_gdata (ecf, xtype, xd, yd, zd, Memr[xr], npts)
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
		        call ecf_gdata (ecf, ytype, xd, yd, zd, Memr[yr], npts)
		        call ecf_title (gt)
			newgraph = YES
		    } else
			call printf ("\007")
		}

	    case 'r': # Redraw
		newgraph = YES

	    case 'c': # Cursor coordinates
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    Memr[wr], npts)
		call printf ("%10.2g %d %10.8g\n")
		    call pargd (xd[i])
		    call pargd (yd[i])
		    call pargd (zd[i])

	    case 'd': # Delete
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    Memr[wr], npts)
		if (i > 0)
		    wd[i] = Memr[wr+i-1]

	    case 'u': # Undelete
		i = ecf_nearest (gp, gt, wx, wy, wcs, key, Memr[xr], Memr[yr],
		    Memr[wr], npts)
		if (i > 0)
		    wd[i] = Memr[wr+i-1]

	    case 'f': # Fit
		call printf ("Fitting ...")
		call flush (STDOUT)
		call ecf_solve (ecf, xd, yd, zd, wd, npts)
		call ecf_gdata (ecf, xtype, xd, yd, zd, Memr[xr], npts)
		call ecf_gdata (ecf, ytype, xd, yd, zd, Memr[yr], npts)
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
		call ecf_graph (gp, gt, Memr[xr], Memr[yr], Memr[wr], npts)
		newgraph = NO
	    }
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	call sfree (sp)
end

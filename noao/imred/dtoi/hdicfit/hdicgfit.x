include	<error.h>
include	<pkg/gtools.h>
include	<mach.h>
include	<gset.h>
include	"hdicfit.h"

define	HELP		"noao$lib/scr/hdicgfit.key"
define	PROMPT		"hdicfit options"
define	EB_WTS		10
define	EB_SDEV		11

# ICG_FIT -- Interactive curve fitting with graphics.  This is the main
# entry point for the interactive graphics part of the icfit package.

procedure icg_fitd (ic, gp, cursor, gt, cv, density, loge, owts, osdev, npts)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
char	cursor[ARB]		# GIO cursor input
pointer	gt			# GTOOLS pointer
pointer	cv			# CURFIT pointer
double	density[npts]		# Original density, not fog subtracted
double	loge[npts]		# Original log exposure values
double	owts[npts]		# Original weights array
double	osdev[npts]		# Original standard deviation array
int	npts			# Number of points

real	wx, wy, wwy, oldx, oldy
int	wcs, key, nptso
char	cmd[SZ_LINE]

int	i, newgraph, axes[2], linetype
double	x1, newwt
pointer	userwts, x, y, wts, den, whydel, sdev, ebw

int	clgcur(), stridxs(), scan(), nscan()
int	icg_nearestd(), gstati()
double	dcveval()
errchk	ic_fitd, malloc
define	redraw_ 91

begin
	# Allocate memory for the fit and a copy of the weights.
	# The weights are copied because they are changed when points are
	# deleted.  The input x is the untransformed density, and is used
	# to generate other types of transforms.  Points can be added to
	# the sample, so the y array and weights array can change as well.
	# The original number of points is also remembered.

	call malloc (userwts, npts, TY_DOUBLE)
	call malloc (x,       npts, TY_DOUBLE)
	call malloc (y,       npts, TY_DOUBLE)
	call malloc (wts,     npts, TY_DOUBLE)
	call malloc (den,     npts, TY_DOUBLE)
	call malloc (whydel,  npts, TY_INT)
	call malloc (sdev,    npts, TY_DOUBLE)
	call malloc (ebw,     npts, TY_DOUBLE)

	call amovd (owts,     Memd[userwts], npts)
	call amovd (owts,     Memd[wts],     npts)
	call amovd (loge,     Memd[y],       npts)
	call amovd (density,  Memd[den],     npts)
	call amovki (NDELETE, Memi[whydel],  npts)
	call amovd (osdev,    Memd[sdev],    npts)
	nptso = npts

	# Initialize
	IC_OVERPLOT(ic) = NO
	IC_NEWX(ic) = YES
	IC_NEWY(ic) = YES
	IC_NEWWTS(ic) = YES
	IC_NEWFUNCTION(ic) = YES
	IC_NEWTRANSFORM(ic) = YES
	IC_UPDATE(ic) = YES
	IC_EBARS(ic) = EB_SDEV

	# Read cursor commands.

	key = 'f'
	axes[1] = IC_AXES(ic, IC_GKEY(ic), 1)
	axes[2] = IC_AXES(ic, IC_GKEY(ic), 2)

	repeat {
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case 'q': # Terminate cursor loop
		break

	    case ':': # List or set parameters
		if (stridxs ("/", cmd) == 1)
	            call gt_colon (cmd, gp, gt, newgraph)
		else
		    call icg_colond (ic, cmd, gp, gt, cv, Memd[x], 
			Memd[y], Memd[wts], npts)

		if (IC_RESET(ic) == YES) {
		    npts = nptso
		    call amovd (owts, Memd[userwts], npts)
		    call amovd (owts, Memd[wts], npts)
		    call amovd (loge, Memd[y], npts)
		    call amovd (density, Memd[den], npts)
		    call amovki (NDELETE, Memi[whydel], npts)
		    call amovd (osdev, Memd[sdev], npts)
		    call hdic_init (density, npts, 0.0)
		}

		# See if user wants to quit without updating
		if (IC_UPDATE(ic) == NO) {
		    call mfree (x, TY_DOUBLE)
		    call mfree (y, TY_DOUBLE)
		    call mfree (wts, TY_DOUBLE)
		    call mfree (userwts, TY_DOUBLE)
		    call mfree (den, TY_DOUBLE)
		    call mfree (sdev, TY_DOUBLE)
		    return
		}

	    case 'a': # Add data points to the sample.  This is only possible 
		      # from an HD curve plot.

		if ((IC_AXES (ic, IC_GKEY(ic), 1) == 'y') &&
		    (IC_AXES (ic, IC_GKEY(ic), 2) == 'u')) {

		    # Query for weight after plotting current location
		    # call gt_plot (gp, gt, wx, wy, 1)
		    call gmark (gp, wx, wy, GM_CIRCLE, 2.0, 2.0)
		    newwt = 1.0D0
		    call printf ("Enter weight of new point (%g): ")
			call pargd (newwt)
		    call flush (STDOUT)
		    if (scan() != EOF) {
		        call pargd (x1)
		        if (nscan() == 1) {
			    if (!IS_INDEFD (x1)) {
			        newwt = x1
			    }
		        }
		    }

		} else {
		    call eprintf ("Points can be added only from an HD Curve\n")
		    next
		}

		# Add fog into "density above fog" value read from cursor
		wwy = wy + IC_FOG (ic)
		if (wwy < 0.0) {
		    call eprintf (
			"New density (%g) is below fog and will not be added\n")
			    call pargr (wwy)
		    next
		}

		# Add point into sample
		call eprintf ("New Point: density above fog = %.4f, log ")
		    call pargr (wwy)
		call eprintf ("exposure = %.4f, weight = %.4f\n")
		    call pargr (wx)
		    call pargd (newwt)

		call hdic_addpoint (ic, wwy, wx, newwt, den, y, wts, userwts, 
		    x, whydel, sdev, npts)

		call realloc (ebw, npts, TY_DOUBLE)

		call hdic_transform (ic, Memd[den], Memd[userwts], Memd[x], 
		    Memd[wts], Memi[whydel], npts)

	    case 'c': # Print the positions of data points.
		i = icg_nearestd (ic, gp, gt, cv, Memd[x], Memd[y], npts, 
		    wx, wy)

	    	if (i != 0) {
		    call printf ("den= %7.4g x= %7.4g  exp= %7.4g  fit= %7.4g")
			call pargd (Memd[den+i-1])
			call pargd (Memd[x+i-1])
			call pargd (Memd[y+i-1])
			call pargd (dcveval (cv, Memd[x+i-1]))
		}

	    case 'd': # Delete data points.
		call icg_deleted (ic, gp, gt, cv, Memd[x], Memd[y], Memd[wts], 
		    npts, wx, wy)

	    case 'f': # Fit the function and reset the flags.
		iferr {
		    # Copy new transformed vector, if necessary
		    if (IC_NEWTRANSFORM(ic) == YES || IC_NEWFOG(ic) == YES) 
			call hdic_transform (ic, Memd[den], Memd[userwts], 
			    Memd[x], Memd[wts], Memi[whydel], npts)

		    call ic_fitd (ic, cv, Memd[x], Memd[y], Memd[wts], npts, 
			IC_NEWX(ic), IC_NEWY(ic), IC_NEWWTS(ic), 
			    IC_NEWFUNCTION(ic))

		    IC_NEWX(ic) = NO
		    IC_NEWY(ic) = NO
		    IC_NEWWTS(ic) = NO
		    IC_NEWFUNCTION(ic) = NO
		    IC_NEWTRANSFORM(ic) = NO
		    IC_FITERROR(ic) = NO
		    IC_NEWFOG(ic) = NO
		    newgraph = YES
		} then {
		    IC_FITERROR(ic) = YES
		    call erract (EA_WARN)
		    newgraph = NO
		}

	    case 'g':	# Set graph axes types.
		call printf ("Graph key to be defined: ")
		call flush (STDOUT)
		if (scan() == EOF)
		    goto redraw_
		call gargc (cmd[1])

		switch (cmd[1]) {
		case '\n':
		case 'h', 'i', 'j', 'k', 'l':
		    switch (cmd[1]) {
		    case 'h':
		        key = 1
		    case 'i':
		        key = 2
		    case 'j':
		        key = 3
		    case 'k':
		        key = 4
		    case 'l':
		        key = 5
		    }

		    call printf ("Set graph axes types (%c, %c): ")
		        call pargc (IC_AXES(ic, key, 1))
		        call pargc (IC_AXES(ic, key, 2))
		    call flush (STDOUT)
		    if (scan() == EOF)
		        goto redraw_
		    call gargc (cmd[1])

		    switch (cmd[1]) {
		    case '\n':
		    default:
		        call gargc (cmd[2])
		        call gargc (cmd[2])
		        if (cmd[2] != '\n') {
			    IC_AXES(ic, key, 1) = cmd[1]
			    IC_AXES(ic, key, 2) = cmd[2]
		        }
		    }
		default:
		    call printf ("Not a graph key")
		}

	    case 'h':
		if (IC_GKEY(ic) != 1) {
		    IC_GKEY(ic) = 1
		    newgraph = YES
		}

	    case 'i':
		if (IC_GKEY(ic) != 2) {
		    IC_GKEY(ic) = 2
		    newgraph = YES
		}

	    case 'j':
		if (IC_GKEY(ic) != 3) {
		    IC_GKEY(ic) = 3
		    newgraph = YES
		}

	    case 'k':
		if (IC_GKEY(ic) != 4) {
		    IC_GKEY(ic) = 4
		    newgraph = YES
		}

	    case 'l':
		if (IC_GKEY(ic) != 5) {
		    IC_GKEY(ic) = 5
		    newgraph = YES
		}

	    case 'o': # Set overplot flag
		IC_OVERPLOT(ic) = YES

	    case 'r': # Redraw the graph
		newgraph = YES

	    case 'u': # Undelete data points.
		call icg_undeleted (ic, gp, gt, cv, Memd[x], Memd[y], 
		    Memd[wts], Memd[userwts], npts, wx, wy)

	    case 'w':  # Window graph
		call gt_window (gt, gp, cursor, newgraph)

	    case 'x': # Reset the value of the x point.
		i = icg_nearestd (ic, gp, gt, cv, Memd[x], Memd[y], npts, wx, 
		    wy)

	    	if (i != 0) {
		    call printf ("Enter new x (%g): ")
			call pargd (Memd[x+i-1])
		    call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargd (x1)
		        if (nscan() == 1) {
			    if (!IS_INDEF (x1)) {
				oldx = Memd[x+i-1]
				oldy = Memd[y+i-1]
			        Memd[x+i-1] = x1
				call hd_redraw (gp, oldx, oldy, x1, oldy)
			        IC_NEWX(ic) = YES
			    }
			}
		    }
		}

	    case 'y': # Reset the value of the y point.
		i = icg_nearestd (ic, gp, gt, cv, Memd[x], Memd[y], npts, wx, 
		    wy)

	    	if (i != 0) {
		    call printf ("Enter new y (%g): ")
			call pargd (Memd[y+i-1])
		    call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargd (x1)
		        if (nscan() == 1) {
			    if (!IS_INDEF (x1)) {
				oldx = Memd[x+i-1]
				oldy = Memd[y+i-1]
			        Memd[y+i-1] = x1
				call hd_redraw (gp, oldx, oldy, oldx, x1)
			        IC_NEWY(ic) = YES
			    }
			}
		    }
		}

	    case 'z': # Reset the weight value of the nearest point
		i = icg_nearestd (ic, gp, gt, cv, Memd[x], Memd[y], npts, wx, 
		    wy)

	    	if (i != 0) {
		    call printf ("Enter new weight (%g): ")
			call pargd (Memd[wts+i-1])
		    call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargd (x1)
		        if (nscan() == 1) {
			    if (!IS_INDEF (x1)) {
			        Memd[wts+i-1] = x1
			        IC_NEWWTS(ic) = YES
			    }
			}
		    }
		}

	    default: # Let the user decide on any other keys.
		call icg_user (ic, gp, gt, cv, wx, wy, wcs, key, cmd)
	    }

	    # Redraw the graph if necessary.
redraw_	    if (newgraph == YES) {
		if (IC_AXES(ic, IC_GKEY(ic), 1) != axes[1]) {
		    axes[1] = IC_AXES(ic, IC_GKEY(ic), 1)
		    call gt_setr (gt, GTXMIN, INDEF)
		    call gt_setr (gt, GTXMAX, INDEF)
		}
		if (IC_AXES(ic, IC_GKEY(ic), 2) != axes[2]) {
		    axes[2] = IC_AXES(ic, IC_GKEY(ic), 2)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		}

		# Overplot with a different line type
		if (IC_OVERPLOT(ic) == YES)
		    linetype = min ((gstati (gp, G_PLTYPE) + 1), 4)
		else
		    linetype = GL_SOLID
		call gseti (gp, G_PLTYPE, linetype)

		call hdic_ebw (ic, Memd[den], Memd[x], Memd[sdev], Memd[ebw], 
		    npts)

	    	call icg_graphd (ic, gp, gt, cv, Memd[x], Memd[y], Memd[wts], 
		    Memd[ebw], npts)

		newgraph = NO
	    }
	} until (clgcur (cursor, wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	call mfree (x, TY_DOUBLE)
	call mfree (y, TY_DOUBLE)
	call mfree (wts, TY_DOUBLE)
	call mfree (userwts, TY_DOUBLE)
	call mfree (den, TY_DOUBLE)
end

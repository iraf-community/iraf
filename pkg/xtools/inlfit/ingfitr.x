include	<error.h>
include	<mach.h>
include	<pkg/gtools.h>
include <math/nlfit.h>
include	<pkg/inlfit.h>


# IN_GFIT -- Fit a function using non-linear least squares. The function
# can have an arbitrary number of independent variables. This is the main
# entry point for the interactive part of the INLFIT package.


procedure ing_fitr (in, gp, cursor, gt, nl, x, y, wts, names, npts, nvars,
	len_name, wtflag, stat)

pointer	in			# INLFIT pointer
pointer	gp			# GIO pointer
char	cursor[ARB]		# GIO cursor input
pointer	gt			# GTOOLS pointer
pointer	nl			# NLFIT pointer
real	x[ARB]			# independent variables (npts * nvars)
real	y[ARB]			# dependent variables
real	wts[ARB]		# weigths
char	names[ARB]		# star ids
int	npts			# number of points
int	nvars			# number of variables
int	len_name		# length of an object name
int	wtflag			# type of weighting
int	stat			# Error code (output)

int	i, wcs, key, gkey, newgraph
int	xtype, xvar, ytype, yvar, xt, xv, yt, yv
real	fit
pointer	sp, cmd, oldwts, help, prompt
real	wx, wy

int	gt_gcur1(), ing_nearestr(), in_geti()
real	nlevalr()

begin
	# Allocate string space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Allocate and initialize a copy of the weights. A new copy
	# of the weights is used because it is necessary to have the
	# original values to restore them back when the user deletes
	# and undeletes points.

	call salloc (oldwts, npts, TY_REAL)
	call amovr (wts, Memr[oldwts], npts)

	# Allocate space for help page and prompt, and get them.
	call salloc (help, SZ_LINE, TY_CHAR)
	call salloc (prompt, SZ_LINE, TY_CHAR)
	call in_gstr (in, INLHELP, Memc[help], SZ_LINE)
	call in_gstr (in, INLPROMPT, Memc[prompt], SZ_LINE)

	# Initialize INLFIT flags.
	call in_puti (in, INLOVERPLOT, NO)

	# Initialize loop control variables. The first action
	# is to fit the data, in order to have all the fit
	# parameters set.
	key = 'f'
	newgraph = YES

	# Get initial setup for axes.
	gkey = in_geti (in, INLGKEY)
	call in_gkey (in, gkey, INLXAXIS, xtype, xvar)
	call in_gkey (in, gkey, INLYAXIS, xtype, xvar)

	# Loop reading cursor commands.
	repeat {
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, Memc[help], Memc[prompt])

	    case ':': # List or set parameters.
		if (Memc[cmd] == '/')
	            call gt_colon (Memc[cmd], gp, gt, newgraph)
		else
		    call ing_colonr (in, Memc[cmd], gp, gt, nl, x, y, wts,
		        names, npts, nvars, len_name, newgraph)

	    case 'c': # Print the positions and useful info on data points.

		i = ing_nearestr (in, gp, gt, nl, x, y, npts, nvars, wx, wy)
	    	if (i != 0) {
		    fit = nlevalr (nl, x[(i-1)*nvars+1], nvars)
		    call printf (
		        "%d %s x=%g y=%g func=%g fit=%g, resid=%g\n")
			call pargi (i)
			call pargstr (names[(i-1)*len_name+1])
			call pargr (wx)
			call pargr (wy)
			call pargr (y[i])
			call pargr (fit)
			call pargr (y[i] - fit)
		}

	    case 'd': # Delete data points.
		call ing_deleter (in, gp, gt, nl, x, y, wts, npts, nvars,
		    wx, wy)

	    case 'f': # Fit the function.

		# Fit.
		do i = 1, npts {
		    if (wts[i] > real(0.0))
		        wts[i] = Memr[oldwts+i-1]
		}
		call in_fitr (in, nl, x, y, wts, npts, nvars, wtflag, stat)

		newgraph = YES

	    case 'g':	# Set graph axistype types.
		call ing_defkey (in, nvars, newgraph)

	    case 'h':
		if (in_geti (in, INLGKEY) != 1) {
		    call in_puti (in, INLGKEY, 1)
		    newgraph = YES
		}

	    case 'i':
		if (in_geti (in, INLGKEY) != 2) {
		    call in_puti (in, INLGKEY, 2)
		    newgraph = YES
		}

	    case 'j':
		if (in_geti (in, INLGKEY) != 3) {
		    call in_puti (in, INLGKEY, 3)
		    newgraph = YES
		}

	    case 'k':
		if (in_geti (in, INLGKEY) != 4) {
		    call in_puti (in, INLGKEY, 4)
		    newgraph = YES
		}

	    case 'l':
		if (in_geti (in, INLGKEY) != 5) {
		    call in_puti (in, INLGKEY, 5)
		    newgraph = YES
		}

	    case 'o': # Set the overplot flag.
		call in_puti (in, INLOVERPLOT, YES)

	    case 'r': # Redraw the graph.
		newgraph = YES

	    case 't': # Toggle overplot fit flag.
		if (in_geti (in, INLPLOTFIT) == YES)
		    call in_puti (in, INLPLOTFIT, NO)
		else
		    call in_puti (in, INLPLOTFIT, YES)
		newgraph = YES

	    case 'u': # Undelete data points.
		call ing_undeleter (in, gp, gt, nl, x, y, wts, Memr[oldwts],
	     	    npts, nvars, wx, wy)

	    case 'w':  # Window graph.
		call gt_window (gt, gp, cursor, newgraph)

	    case 'I': # Interrupt.
		call fatal (0, "Interrupt")

	    default: # Let the user decide on any other keys.
		call ing_ufit (in, gp, gt, nl, wx, wy, wcs, key, Memc[cmd])
	    }

	    # Redraw the graph if necessary.
	    if (newgraph == YES) {
		gkey = in_geti (in, INLGKEY)
		call in_gkey (in, gkey, INLXAXIS, xt, xv)
		if (xt != xtype || xv != xvar) {
		    call in_gkey (in, gkey, INLXAXIS, xtype, xvar)
		    call gt_setr (gt, GTXMIN, INDEFR)
		    call gt_setr (gt, GTXMAX, INDEFR)
		}
		call in_gkey (in, gkey, INLYAXIS, yt, yv)
		if (xt != xtype || xv != xvar) {
		    call in_gkey (in, gkey, INLYAXIS, ytype, yvar)
		    call gt_setr (gt, GTYMIN, INDEFR)
		    call gt_setr (gt, GTYMAX, INDEFR)
		}
	    	call ing_graphr (in, gp, gt, nl, x, y, wts, npts, nvars)
		newgraph = NO
	    }

	    if (cursor[1] == EOS)
		break

	} until (gt_gcur1 (gt, cursor, wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) == EOF)

	# Free memory.
	call sfree (sp)
end

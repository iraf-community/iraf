include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>
include	"hdicfit.h"

define	MSIZE		2.		# Mark size
define	SZ_TPLOT	7		# String length for plot type
define	EB_WTS		10
define	EB_SDEV		11
define	MAX_SZMARKER	4

# ICG_GRAPH -- Graph data and fit.

procedure icg_graphd (ic, gp, gt, cv, x, y, wts, ebw, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
pointer	cv				# Curfit pointer
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
double	wts[npts]			# Weights
double	ebw[npts]			# Half the error bar width
int	npts				# Number of points

pointer	xout, yout
int	nvalues
errchk	malloc

begin
	call malloc (xout, npts, TY_DOUBLE)
	call malloc (yout, npts, TY_DOUBLE)

	call icg_axesd (ic, gt, cv, 1, x, y, Memd[xout], npts)
	call icg_axesd (ic, gt, cv, 2, x, y, Memd[yout], npts)
	call icg_paramsd (ic, cv, x, y, wts, npts, gt)
	
	call icg_g1d (ic, gp, gt, Memd[xout], Memd[yout], wts, ebw, npts)

	if (npts != IC_NFIT(ic)) {
	    if ((abs (IC_NAVERAGE(ic)) > 1) || (IC_NREJECT(ic) > 0)) {
	        call realloc (xout, IC_NFIT(ic), TY_DOUBLE)
		call realloc (yout, IC_NFIT(ic), TY_DOUBLE)
		call icg_axesd (ic, gt, cv, 1, Memd[IC_XFIT(ic)],
		    Memd[IC_YFIT(ic)], Memd[xout], IC_NFIT(ic))
		call icg_axesd (ic, gt, cv, 2, Memd[IC_XFIT(ic)],
		    Memd[IC_YFIT(ic)], Memd[yout], IC_NFIT(ic))
		call icg_g2d (ic, gp, gt, Memd[xout], Memd[yout],
		    IC_NFIT(ic))
	    }

	} else if (IC_NREJECT(ic) > 0)
	    call icg_g2d (ic, gp, gt, Memd[xout], Memd[yout], npts)

	nvalues = NVALS_FIT
	call icg_gfd (ic, gp, gt, cv, nvalues)

	# Mark the the sample regions.
	call icg_sampled (ic, gp, gt, x, npts, 1)

	call mfree (xout, TY_DOUBLE)
	call mfree (yout, TY_DOUBLE)
end


# ICG_G1D -- Plot data vector.

procedure icg_g1d (ic, gp, gt, x, y, wts, ebw, npts)

pointer	ic		# Pointer to ic structure
pointer	gp		# Pointer to graphics stream
pointer	gt		# Pointer to gtools structure
double	x[npts]		# Array of independent variables
double	y[npts]		# Array of dependent variables
double	wts[npts]	# Array of weights
double	ebw[npts]	# Error bar half width in WCS (positive density)
int	npts		# Number of points

pointer	sp, xr, yr, sz, szmk, gt1, gt2
char	tplot[SZ_TPLOT]
int	i, xaxis, yaxis, symbols[3], markplot
real	size, rmin, rmax, big
bool	fp_equald(), streq(), fp_equalr()
int	strncmp()
data	symbols/GM_PLUS, GM_BOX, GM_CIRCLE/
include	"hdic.com"

begin
	call smark (sp)
	call salloc (xr,   npts, TY_REAL)
	call salloc (yr,   npts, TY_REAL)
	call salloc (sz,   npts, TY_REAL)
	call salloc (szmk, npts, TY_REAL)

	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	xaxis = (IC_AXES (ic, IC_GKEY(ic), 1))
	yaxis = (IC_AXES (ic, IC_GKEY(ic), 2))

	# Set up gtools structure for deleted (gt1) and added (gt2) points
	call gt_copy (gt, gt1)
	call gt_setr (gt1, GTXSIZE, MSIZE)
	call gt_setr (gt1, GTYSIZE, MSIZE)
	call gt_sets (gt1, GTMARK,  "cross")

	call gt_copy (gt, gt2)
	call gt_setr (gt2, GTXSIZE, MSIZE)
	call gt_setr (gt2, GTYSIZE, MSIZE)
	call gt_sets (gt2, GTMARK,  "circle")

	markplot = NO
	call gt_gets (gt, GTTYPE, tplot, SZ_TPLOT)
	if (strncmp (tplot, "mark", 4) == 0)
	    markplot = YES

	if (IC_OVERPLOT(ic) == NO) {
	    # Start a new plot
	    call gclear (gp)

	    # Set the graph scale and axes
	    call gascale (gp, Memr[xr], npts, 1)
	    call gascale (gp, Memr[yr], npts, 2)

	    # If plotting HD curve, set wy2 to maxden, which may have
	    # been updated if a new endpoint was added.

	    if (xaxis == 'y' && yaxis == 'u')
		call gswind (gp, INDEF, INDEF, INDEF, real (maxden))

	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	# Calculate size of markers if error bars are being used.  If the
	# weights are being used as the marker size, they are first scaled
	# between 1.0 and the maximum marker size.

	call gt_gets (gt, GTMARK, tplot, SZ_TPLOT)
	if (streq (tplot, "hebar") || streq (tplot, "vebar")) {
	    if (IC_EBARS(ic) == EB_WTS) {
		call achtdr (wts, Memr[sz], npts)
		call alimr  (Memr[sz], npts, rmin, rmax)
		if (fp_equalr (rmin, rmax))
		    call amovr (Memr[sz], Memr[szmk], npts)
		else {
		    big = real  (MAX_SZMARKER)
		    call amapr  (Memr[sz], Memr[szmk], npts,rmin,rmax,1.0, big)
		}
	    } else {
		call achtdr (ebw, Memr[sz], npts)
		call hd_szmk (gp, gt, xaxis, yaxis, tplot, Memr[sz], 
		    Memr[szmk], npts)
	    }
	} else
	    call amovkr (MSIZE, Memr[szmk], npts)

	do i = 1, npts {
	    # Check for deleted point
	    if (fp_equald (wts[i], 0.0D0))
	        call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1)

	    # Check for added point
	    else if (fp_equald (ebw[i], ADDED_PT))
	        call gt_plot (gp, gt2, Memr[xr+i-1], Memr[yr+i-1], 1)

	    else {
	        size = Memr[szmk+i-1]
	        call gt_setr (gt, GTXSIZE, size)
	        call gt_setr (gt, GTYSIZE, size)
                call gt_plot (gp, gt, Memr[xr+i-1], Memr[yr+i-1], 1)
	    }
	}
	
	IC_OVERPLOT(ic) = NO
	call sfree (sp)
end


# HD_SZMK -- Calculate size of error bar markers.  This procedure is
# called when the marker type is hebar or vebar and the marker size is
# to be the error in the point, not its weight in the fit.

procedure hd_szmk (gp, gt, xaxis, yaxis, mark, insz, outsz, npts)

pointer	gp			# Pointer to gio structure
pointer	gt			# Pointer to gtools structure
int	xaxis, yaxis		# Codes for x and y axis types
char	mark[SZ_TPLOT]		# Type of marker to use
real	insz[npts]		# Standard deviations in WCS units
real	outsz[npts]		# Output size array in NDC units
int	npts			# Number of points in arrays

int	gt_geti()
char	tplot[SZ_TPLOT]
real	dx, dy
bool	streq()

begin
	# Check validity of axis types
	if (xaxis != 'x' && xaxis != 'u' && yaxis != 'x' && yaxis != 'u') {
	    call eprintf ("Choose graph type with axes 'u' or 'x'; ")
	    call eprintf ("Using marker size 2.0\n")
	    call amovkr (MSIZE, outsz, npts)
	    return
	}

	call gt_gets (gt, GTMARK, tplot, SZ_TPLOT)
	if (streq (tplot, "hebar")) {
	    if (yaxis == 'x' || yaxis == 'u') {
	        call gt_sets (gt, GTMARK, "vebar")
	        call eprintf ("Marker switched to vebar\n")
		# call flush (STDOUT)
	    }
	} else if (streq (tplot, "vebar")) {
	    if (xaxis == 'x' || xaxis == 'u') {
	        call gt_sets (gt, GTMARK, "hebar")
	        call eprintf ("Marker switched to hebar\n")
		# call flush (STDOUT)
	    }
	}

	# Need to scale standard deviation from density to NDC units
        call ggscale (gp, 0.0, 0.0, dx, dy)
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call adivkr (insz, dx, outsz, npts)
	else
	    call adivkr (insz, dy, outsz, npts)
end


# ICG_G2D -- Show sample range and rejected data on plot.

procedure icg_g2d (ic, gp, gt, x, y, npts)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
double	x[npts], y[npts]	# Data points
int	npts			# Number of data points

int	i
pointer	sp, xr, yr, gt1

begin
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")

	# Mark the sample points.

	if (abs (IC_NAVERAGE(ic)) > 1) {
	    call gt_sets (gt1, GTMARK, "plus")
	    call gt_setr (gt1, GTXSIZE, real (-abs (IC_NAVERAGE(ic))))
	    call gt_setr (gt1, GTYSIZE, 1.)
	    call gt_plot (gp, gt1, Memr[xr], Memr[yr], npts)
	}

	# Mark the rejected points.

	if (IC_NREJECT(ic) > 0) {
	    call gt_sets (gt1, GTMARK, "diamond")
	    call gt_setr (gt1, GTXSIZE, MSIZE)
	    call gt_setr (gt1, GTYSIZE, MSIZE)
	    do i = 1, npts {
		if (Memi[IC_REJPTS(ic)+i-1] == YES)
	            call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1)
	    }
	}

	call gt_free (gt1)
	call sfree (sp)
end


# ICG_GF[RD] -- Overplot the fit on a graph of the data points.  A vector
# is constructed and evaluated, then plotted.

procedure icg_gfd (ic, gp, gt, cv, npts)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOL pointer
pointer	cv			# CURFIT pointer
int	npts			# Number of points to plot

pointer	sp, xr, yr, x, y, xo, yo, gt1
int	gstati()

begin
	call smark (sp)

	if (IC_FITERROR(ic) == YES)
	    return

	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (x,  npts, TY_DOUBLE)
	call salloc (y,  npts, TY_DOUBLE)
	call salloc (xo, npts, TY_DOUBLE)
	call salloc (yo, npts, TY_DOUBLE)

	# Calculate big vector of independent variable values.  Note value
	# of npts can change with this call.
	call hdic_gvec (ic, Memd[x], npts, IC_TRANSFORM(ic))

	# Calculate vector of fit values. 
	call dcvvector (cv, Memd[x], Memd[y], npts)

	# Convert to user function or transpose axes.  Change type to reals
	# for plotting.
	call icg_axesd (ic, gt, cv, 1, Memd[x], Memd[y], Memd[xo], npts)
	call icg_axesd (ic, gt, cv, 2, Memd[x], Memd[y], Memd[yo], npts)
	call achtdr (Memd[xo], Memr[xr], npts)
	call achtdr (Memd[yo], Memr[yr], npts)

	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "line")
	call gt_seti (gt1, GTLINE, gstati (gp, G_PLTYPE))
	call gt_plot (gp, gt1, Memr[xr], Memr[yr], npts)
	call gt_free (gt1)

	call sfree (sp)
end

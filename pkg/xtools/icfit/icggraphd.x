# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<pkg/gtools.h>
include	"names.h"
include	"icfit.h"

define	NGRAPH		100		# Number of fit points to graph
define	MSIZE		2.		# Mark size

# ICG_GRAPH -- Graph data and fit.

procedure icg_graphd (ic, gp, gt, cv, x, y, wts, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
pointer	cv				# Curfit pointer
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
double	wts[npts]			# Weights
int	npts				# Number of points

pointer	xout, yout
real	size

begin
	call malloc (xout, npts, TY_DOUBLE)
	call malloc (yout, npts, TY_DOUBLE)
	call icg_axesd (ic, gt, cv, 1, x, y, Memd[xout], npts)
	call icg_axesd (ic, gt, cv, 2, x, y, Memd[yout], npts)
	call icg_paramsd (ic, cv, x, y, wts, npts, gt)

	call icg_g1d (ic, gp, gt, Memd[xout], Memd[yout], wts, npts)

 	# Symbol size for averaged ranges.
 	size = abs(IC_NAVERAGE(ic) * (Memd[xout+npts-1] - Memd[xout]) / 
 	       float(npts))
 
	if (npts != IC_NFIT(ic)) {
	    if ((abs (IC_NAVERAGE(ic)) > 1) || (IC_NREJECT(ic) > 0)) {
	        call realloc (xout, IC_NFIT(ic), TY_DOUBLE)
		call realloc (yout, IC_NFIT(ic), TY_DOUBLE)
		call icg_axesd (ic, gt, cv, 1, Memd[IC_XFIT(ic)],
		    Memd[IC_YFIT(ic)], Memd[xout], IC_NFIT(ic))
		call icg_axesd (ic, gt, cv, 2, Memd[IC_XFIT(ic)],
		    Memd[IC_YFIT(ic)], Memd[yout], IC_NFIT(ic))
		call icg_g2d (ic, gp, gt, Memd[xout], Memd[yout],
		    IC_NFIT(ic), size)
	    }

	} else if (IC_NREJECT(ic) > 0)
	    call icg_g2d (ic, gp, gt, Memd[xout], Memd[yout], npts, size)

	call icg_gfd (ic, gp, gt, cv, max (npts, NGRAPH))

	# Mark the the sample regions.

	call icg_sampled (ic, gp, gt, x, npts, 1)

	# Send the wcs to the gui.
	call ic_gui (ic, "wcs")

	call mfree (xout, TY_DOUBLE)
	call mfree (yout, TY_DOUBLE)
end

procedure icg_g1d (ic, gp, gt, x, y, wts, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	x[npts]				# Ordinates
double	y[npts]				# Abscissas
double	wts[npts]			# Weights
int	npts				# Number of points

int	i
pointer	sp, xr, yr, xr1, yr1, gt1

begin
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (xr1, 2, TY_REAL)
	call salloc (yr1, 2, TY_REAL)
	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")
	call gt_sets (gt1, GTMARK, "cross")

	if (IC_OVERPLOT(ic) == NO) {
	    # Start a new plot.

	    call gclear (gp)

	    # Set the graph scale and axes.

	    call gascale (gp, Memr[xr], npts, 1)
	    call gascale (gp, Memr[yr], npts, 2)
	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	if (IC_OVERPLOT(ic) == NO) {
	    Memr[xr1] = Memr[xr]
	    Memr[yr1] = Memr[yr]
	    do i = 1, npts {
		if (wts[i] == 0.) {
		    call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1) 
		} else {
		    Memr[xr1+1] = Memr[xr+i-1]
		    Memr[yr1+1] = Memr[yr+i-1]
		    call gt_plot (gp, gt, Memr[xr1], Memr[yr1], 2)
		    Memr[xr1] = Memr[xr1+1]
		    Memr[yr1] = Memr[yr1+1]
		}
	    }
	}

	# Reset status flags.

	IC_OVERPLOT(ic) = NO

	call sfree (sp)
	call gt_free (gt1)
end

procedure icg_g2d (ic, gp, gt, x, y, npts, size)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
double	x[npts], y[npts]	# Data points
int	npts			# Number of data points
real	size			# Symbol size

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
 	    call gt_setr (gt1, GTXSIZE, -size)
	    call gt_setr (gt1, GTYSIZE, 1.)
	    call gt_plot (gp, gt1, Memr[xr], Memr[yr], npts)
	}

	# Mark the rejected points.

	if (IC_NREJECT(ic) > 0 && IC_MARKREJ(ic) == YES) {
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

procedure icg_gfd (ic, gp, gt, cv, npts)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOL pointer
pointer	cv			# CURFIT pointer
int	npts			# Number of points to plot

pointer	sp, xr, yr, x, y, xo, yo, gt1
int	i
double	dx

begin
	if (IC_FITERROR(ic) == YES)
	    return

	call smark (sp)

	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (x,  npts, TY_DOUBLE)
	call salloc (y,  npts, TY_DOUBLE)
	call salloc (xo, npts, TY_DOUBLE)
	call salloc (yo, npts, TY_DOUBLE)

	# Generate vector of independent variable values
	dx = (IC_XMAX(ic) - IC_XMIN(ic)) / (npts - 1)
	do i = 1, npts
	    Memd[x+i-1] = IC_XMIN(ic) + (i-1) * dx

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
	call gt_seti (gt1, GTLINE, GL_DASHED)
	call gt_seti (gt1, GTCOLOR, max (0, min (9, IC_COLOR(ic))))
	call gt_plot (gp, gt1, Memr[xr], Memr[yr], npts)
	call gt_free (gt1)

	call sfree (sp)
end

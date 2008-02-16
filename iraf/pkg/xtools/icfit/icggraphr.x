# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<pkg/gtools.h>
include	"names.h"
include	"icfit.h"

define	NGRAPH		100		# Number of fit points to graph
define	MSIZE		2.		# Mark size

# ICG_GRAPH -- Graph data and fit.

procedure icg_graphr (ic, gp, gt, cv, x, y, wts, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
pointer	cv				# Curfit pointer
real	x[npts]				# Independent variable
real	y[npts]				# Dependent variable
real	wts[npts]			# Weights
int	npts				# Number of points

pointer	xout, yout
real	size

begin
	call malloc (xout, npts, TY_REAL)
	call malloc (yout, npts, TY_REAL)
	call icg_axesr (ic, gt, cv, 1, x, y, Memr[xout], npts)
	call icg_axesr (ic, gt, cv, 2, x, y, Memr[yout], npts)
	call icg_paramsr (ic, cv, x, y, wts, npts, gt)

	call icg_g1r (ic, gp, gt, Memr[xout], Memr[yout], wts, npts)

 	# Symbol size for averaged ranges.
 	size = abs(IC_NAVERAGE(ic) * (Memr[xout+npts-1] - Memr[xout]) / 
 	       float(npts))
 
	if (npts != IC_NFIT(ic)) {
	    if ((abs (IC_NAVERAGE(ic)) > 1) || (IC_NREJECT(ic) > 0)) {
	        call realloc (xout, IC_NFIT(ic), TY_REAL)
		call realloc (yout, IC_NFIT(ic), TY_REAL)
		call icg_axesr (ic, gt, cv, 1, Memr[IC_XFIT(ic)],
		    Memr[IC_YFIT(ic)], Memr[xout], IC_NFIT(ic))
		call icg_axesr (ic, gt, cv, 2, Memr[IC_XFIT(ic)],
		    Memr[IC_YFIT(ic)], Memr[yout], IC_NFIT(ic))
		call icg_g2r (ic, gp, gt, Memr[xout], Memr[yout],
		    IC_NFIT(ic), size)
	    }

	} else if (IC_NREJECT(ic) > 0)
	    call icg_g2r (ic, gp, gt, Memr[xout], Memr[yout], npts, size)

	call icg_gfr (ic, gp, gt, cv, max (npts, NGRAPH))

	# Mark the the sample regions.

	call icg_sampler (ic, gp, gt, x, npts, 1)

	# Send the wcs to the gui.
	call ic_gui (ic, "wcs")

	call mfree (xout, TY_REAL)
	call mfree (yout, TY_REAL)
end

procedure icg_g1r (ic, gp, gt, x, y, wts, npts)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
real	x[npts]				# Ordinates
real	y[npts]				# Abscissas
real	wts[npts]			# Weights
int	npts				# Number of points

int	i
pointer	sp, xr, yr, xr1, yr1, gt1

begin
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (xr1, 2, TY_REAL)
	call salloc (yr1, 2, TY_REAL)
	call achtrr (x, Memr[xr], npts)
	call achtrr (y, Memr[yr], npts)

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

procedure icg_g2r (ic, gp, gt, x, y, npts, size)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	x[npts], y[npts]	# Data points
int	npts			# Number of data points
real	size			# Symbol size

int	i
pointer	sp, xr, yr, gt1

begin
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call achtrr (x, Memr[xr], npts)
	call achtrr (y, Memr[yr], npts)

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

procedure icg_gfr (ic, gp, gt, cv, npts)

pointer	ic			# ICFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOL pointer
pointer	cv			# CURFIT pointer
int	npts			# Number of points to plot

pointer	sp, xr, yr, x, y, xo, yo, gt1
int	i
real	dx

begin
	if (IC_FITERROR(ic) == YES)
	    return

	call smark (sp)

	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (x,  npts, TY_REAL)
	call salloc (y,  npts, TY_REAL)
	call salloc (xo, npts, TY_REAL)
	call salloc (yo, npts, TY_REAL)

	# Generate vector of independent variable values
	dx = (IC_XMAX(ic) - IC_XMIN(ic)) / (npts - 1)
	do i = 1, npts
	    Memr[x+i-1] = IC_XMIN(ic) + (i-1) * dx

	# Calculate vector of fit values. 
	call rcvvector (cv, Memr[x], Memr[y], npts)

	# Convert to user function or transpose axes.  Change type to reals
	# for plotting.
	call icg_axesr (ic, gt, cv, 1, Memr[x], Memr[y], Memr[xo], npts)
	call icg_axesr (ic, gt, cv, 2, Memr[x], Memr[y], Memr[yo], npts)
	call achtrr (Memr[xo], Memr[xr], npts)
	call achtrr (Memr[yo], Memr[yr], npts)

	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "line")
	call gt_seti (gt1, GTLINE, GL_DASHED)
	call gt_seti (gt1, GTCOLOR, max (0, min (9, IC_COLOR(ic))))
	call gt_plot (gp, gt1, Memr[xr], Memr[yr], npts)
	call gt_free (gt1)

	call sfree (sp)
end

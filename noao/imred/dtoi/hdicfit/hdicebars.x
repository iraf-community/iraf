include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include	"hdicfit.h"

define	MSIZE	2.0

# HDIC_EBARS -- Plot data points with their error bars as markers.  The
# independent variable is fog subtracted transformed density.

procedure hdic_ebars (ic, gp, gt, x, y, wts, ebw, npts)

pointer	ic		# Pointer to ic structure
pointer	gp		# Pointer to graphics stream
pointer	gt		# Pointer to gtools structure
double	x[ARB]		# Array of independent variables
double	y[ARB]		# Array of dependent variables
double	wts[ARB]	# Array of weights
double	ebw[ARB]	# Error bar half width (positive number)
int	npts		# Number of points

pointer	sp, xr, yr, sz
int	orig_mark, i, xaxis, yaxis, mark, added_mark
real	size, dx, dy, szmk
int	gt_geti()
bool	fp_equald()
include	"hdic.com"

begin
	call smark (sp)

	xaxis = (IC_AXES (ic, IC_GKEY(ic), 1))
	yaxis = (IC_AXES (ic, IC_GKEY(ic), 2))

	if (xaxis == 'x' || xaxis == 'u')
	    orig_mark = GM_HEBAR
	else if (yaxis == 'x' || yaxis == 'u')
	    orig_mark = GM_VEBAR
	else {
	    call eprintf ("Choose graph type with axes 'u' or 'x'\n")
	    call sfree (sp)
	    return
	}

	added_mark = GM_CIRCLE

	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (sz, npts, TY_REAL)

	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)
	call achtdr (ebw, Memr[sz], npts)

	if (IC_OVERPLOT(ic) == NO) {
	    # Start a new plot
	    call gclear (gp)

	    # Set the graph scale and axes
	    call gascale (gp, Memr[xr], npts, 1)
	    call gascale (gp, Memr[yr], npts, 2)

	    # If plotting HD curve, set wy2 to maxden, which may have
	    # been updated if a new endpoint was added.

	    if ((IC_AXES (ic, IC_GKEY(ic), 1) == 'y') &&
		(IC_AXES (ic, IC_GKEY(ic), 2) == 'u')) 
		call gswind (gp, INDEF, INDEF, INDEF, real (maxden))

	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	call ggscale (gp, 0.0, 0.0, dx, dy)

	do i = 1, npts {
	    size = Memr[sz+i-1]		# Sizes are WCS units; transform them
	    if (gt_geti (gt, GTTRANSPOSE) == NO) {
	        size = size / dx
		mark = orig_mark
		szmk = size
		# Check for added point
		if (fp_equald (ebw[i], ADDED_PT)) {
		    szmk = MSIZE
		    mark = added_mark
		}

		# Check for deleted point
		if (fp_equald (wts[i], 0.0D0)) {
		    szmk = MSIZE
		    mark = mark + GM_CROSS
		}

	        call gmark (gp, Memr[xr+i-1], Memr[yr+i-1], mark, szmk, szmk)

	    } else {
		size = size / dy
		szmk = size
		mark = orig_mark

		# Check for added point
		if (fp_equald (ebw[i], ADDED_PT)) {
		    szmk = MSIZE
		    mark = added_mark
		}

		# Check for deleted point
		if (fp_equald (wts[i], 0.0D0)) {
		    szmk = MSIZE
		    mark = mark + GM_CROSS
		}

		call gmark (gp, Memr[yr+i-1], Memr[xr+i-1], mark, szmk, szmk)
	    }
	}
	
	IC_OVERPLOT(ic) = NO
	call sfree (sp)
end


# HDIC_EBW -- Calculate error bar width for plotting points.  Width is
# returned in NDC units.

procedure hdic_ebw (ic, density, indv, sdev, ebw, npts)

pointer	ic		# Pointer to ic structure
double	density[ARB]	# Untransformed density NOT fog subtracted
double	indv[ARB]	# Transformed density above fog
double	sdev[ARB]	# Array of standard deviation values, density units
double	ebw[ARB]	# Error bar half width (positive numbers)
int	npts		# Number of data points

double	fog
pointer	sp, denaf, outwe
int	xaxis, yaxis, i
bool	fp_equald()
real	ic_getr()

begin
	xaxis = (IC_AXES (ic, IC_GKEY(ic), 1))
	yaxis = (IC_AXES (ic, IC_GKEY(ic), 2))

	if (xaxis == 'u' || yaxis == 'u') {
	    call amovd (sdev, ebw, npts) 
	    return
	}

	call smark (sp)
	call salloc (denaf, npts, TY_DOUBLE)
	call salloc (outwe, npts, TY_DOUBLE)

	fog = double (ic_getr (ic, "fog"))

	call asubkd (density, fog, Memd[denaf], npts)
	call aaddd  (Memd[denaf], sdev, Memd[denaf], npts)

	call hdic_ebtran (Memd[denaf], Memd[outwe], npts, IC_TRANSFORM(ic))

	# Subtract transformed values to get errors.  Then check for
	# added points, which are flagged with ebw=ADDED_PT.

	call asubd (Memd[outwe], indv, ebw, npts)
	do i = 1, npts {
	    if (fp_equald (sdev[i], ADDED_PT))
		ebw[i] = ADDED_PT
	}

	call sfree (sp)
end


# HDIC_EBTRAN -- Apply transformation, generating a vector of independent
# variables from a density vector.  The independent variable vector is the
# standard deviation values and the output values are the errors.  This
# routine checks for ADDED_PT valued standard deviation, which indicates an
# added point.

procedure hdic_ebtran (density, ind_var, npts, transform)

double	density[npts]		# Density vector - input
double	ind_var[npts]		# Ind variable vector - filled on output
int	npts			# Length of data vectors
int	transform		# Integer code for transform type

int	i
bool	fp_equald()

begin
	switch (transform) {
	case HD_LOGO:
	    do i = 1, npts {
		if (fp_equald (density[i], 0.0))
		    ind_var[i] = 0.0
		else
		    ind_var[i] = log10 ((10. ** density[i]) - 1.0)
	    }
	case HD_K75:
	    do i = 1, npts {
		if (fp_equald (density[i], 0.0))
		    ind_var[i] = 0.0
		else
		    ind_var[i] = density[i] + 0.75*log10(1.- 10.**(-density[i]))
	    }
	case HD_K50:
	    do i = 1, npts {
		if (fp_equald (density[i], 0.0))
		    ind_var[i] = 0.0
		else
		    ind_var[i] = density[i] + 0.50*log10(1.- 10.**(-density[i]))
	    }
	case HD_NONE:
	    call amovd (density, ind_var, npts)
	default:
	    call error (0, "Unrecognized transformation in HDIC_EBTRAN")
	}
end

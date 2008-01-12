include	<pkg/gtools.h>
include	"hdicfit.h"

# ICG_AXES -- Set axes data.
# The applications program may set additional axes types.

procedure icg_axesd (ic, gt, cv, axis, x, y, z, npts)

pointer	ic				# ICFIT pointer
pointer	gt				# GTOOLS pointer
pointer	cv				# CURFIT pointer
int	axis				# Output axis
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
double	z[npts]				# Output values
int	npts				# Number of points

int	i, axistype, gtlabel[2], gtunits[2]
double 	a, b, xmin, xmax
pointer	label, units

double	dcveval(), icg_dvzd()
errchk	adivd()
extern	icg_dvzd()

data	gtlabel/GTXLABEL, GTYLABEL/
data	gtunits/GTXUNITS, GTYUNITS/

begin
	axistype = IC_AXES(ic, IC_GKEY(ic), axis)
	switch (axistype) {
	case 'x':	# Independent variable
	    call gt_sets (gt, gtlabel[axis], Memc[IC_LABELS(ic,1)])
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,1)])
	    call amovd (x, z, npts)
	    call gt_sets (gt, GTSUBTITLE, "")
	case 'y':	# Dependent variable
	    call gt_sets (gt, gtlabel[axis], Memc[IC_LABELS(ic,2)])
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call amovd (y, z, npts)
	    call gt_sets (gt, GTSUBTITLE, "")
	case 'f':	# Fitted values
	    call gt_sets (gt, gtlabel[axis], "fit")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call dcvvector (cv, x, z, npts)
	    call gt_sets (gt, GTSUBTITLE, "")
	case 'r':	# Residuals
	    call gt_sets (gt, gtlabel[axis], "residuals")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call dcvvector (cv, x, z, npts)
	    call asubd (y, z, z, npts)
	    call gt_sets (gt, GTSUBTITLE, "")
	case 'd':	# Ratio
	    call gt_sets (gt, gtlabel[axis], "ratio")
	    call gt_sets (gt, gtunits[axis], "")
	    call dcvvector (cv, x, z, npts)
#	    iferr (call adiv$t (y, z, z, npts))
		call advzd (y, z, z, npts, icg_dvzd)
	    call gt_sets (gt, GTSUBTITLE, "")
	case 'n':	# Linear component removed
	    call gt_sets (gt, gtlabel[axis], "non-linear component")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    xmin = IC_XMIN(ic)
	    xmax = IC_XMAX(ic)
	    a = dcveval (cv, double(xmin))
	    b = (dcveval (cv, double(xmax)) - a) / (xmax - xmin)
	    do i = 1, npts
	        z[i] = y[i] - a - b * (x[i] - xmin)
	    call gt_sets (gt, GTSUBTITLE, "")
	default:	# User axes types.
	    call malloc (label, SZ_LINE, TY_CHAR)
	    call malloc (units, SZ_LINE, TY_CHAR)
	    if (axis == 1) {
		call strcpy (Memc[IC_LABELS(ic,1)], Memc[label], SZ_LINE)
		call strcpy (Memc[IC_UNITS(ic,1)], Memc[units], SZ_LINE)
	        call amovd (x, z, npts)
	    } else {
		call strcpy (Memc[IC_LABELS(ic,2)], Memc[label], SZ_LINE)
		call strcpy (Memc[IC_UNITS(ic,2)], Memc[units], SZ_LINE)
	        call amovd (y, z, npts)
	    }
	    call icg_uaxesd (ic, axistype, cv, x, y, z, npts, Memc[label], 
		Memc[units], SZ_LINE)
	    call gt_sets (gt, gtlabel[axis], Memc[label])
	    call gt_sets (gt, gtunits[axis], Memc[units])
	    call gt_sets (gt, GTSUBTITLE, "HD Curve")
	    call mfree (label, TY_CHAR)
	    call mfree (units, TY_CHAR)
	}
end


# ICG_DVZ -- Error action to take on zero division.

double procedure icg_dvzd (x)

double	x			# Numerator

begin
	return (1.)
end

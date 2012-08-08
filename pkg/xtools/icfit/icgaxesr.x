# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/gtools.h>
include	"icfit.h"
include	"names.h"

# ICG_AXES -- Set axes data.
# The applications program may set additional axes types.

procedure icg_axesr (ic, gt, cv, axis, x, y, z, npts)

pointer	ic				# ICFIT pointer
pointer	gt				# GTOOLS pointer
pointer	cv				# CURFIT pointer
int	axis				# Output axis
real	x[npts]				# Independent variable
real	y[npts]				# Dependent variable
real	z[npts]				# Output values
int	npts				# Number of points

int	i, axistype, gtlabel[2], gtunits[2]
real 	a, b, xmin, xmax
pointer	label, units

real	rcveval(), icg_dvzr()
errchk	adivr()
extern	icg_dvzr()

data	gtlabel/GTXLABEL, GTYLABEL/
data	gtunits/GTXUNITS, GTYUNITS/

begin
	axistype = IC_AXES(ic, IC_GKEY(ic), axis)
	switch (axistype) {
	case 'x':	# Independent variable
	    call gt_sets (gt, gtlabel[axis], Memc[IC_LABELS(ic,1)])
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,1)])
	    call amovr (x, z, npts)
	case 'y':	# Dependent variable
	    call gt_sets (gt, gtlabel[axis], Memc[IC_LABELS(ic,2)])
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call amovr (y, z, npts)
	case 'f':	# Fitted values
	    call gt_sets (gt, gtlabel[axis], "fit")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call rcvvector (cv, x, z, npts)
	case 'r':	# Residuals
	    call gt_sets (gt, gtlabel[axis], "residuals")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    call rcvvector (cv, x, z, npts)
	    call asubr (y, z, z, npts)
	case 'd':	# Ratio
	    call gt_sets (gt, gtlabel[axis], "ratio")
	    call gt_sets (gt, gtunits[axis], "")
	    call rcvvector (cv, x, z, npts)
#	    iferr (call adiv$t (y, z, z, npts))
		call advzr (y, z, z, npts, icg_dvzr)
	case 'n':	# Linear component removed
	    call gt_sets (gt, gtlabel[axis], "non-linear component")
	    call gt_sets (gt, gtunits[axis], Memc[IC_UNITS(ic,2)])
	    xmin = IC_XMIN(ic)
	    xmax = IC_XMAX(ic)
	    a = rcveval (cv, real (xmin))
	    b = (rcveval (cv, real (xmax)) - a) / (xmax - xmin)
	    do i = 1, npts
	        z[i] = y[i] - a - b * (x[i] - xmin)
	case 'v':
	    call gt_sets (gt, gtlabel[axis], "Velocity")
	    call gt_sets (gt, gtunits[axis], "km/s")
	    call rcvvector (cv, x, z, npts)
	    do i = 1, npts
	        z[i] = (z[i] - y[i]) / y[i] * 300000.
	default:	# User axes types.
	    call malloc (label, SZ_LINE, TY_CHAR)
	    call malloc (units, SZ_LINE, TY_CHAR)
	    if (axis == 1) {
		call strcpy (Memc[IC_LABELS(ic,1)], Memc[label], SZ_LINE)
		call strcpy (Memc[IC_UNITS(ic,1)], Memc[units], SZ_LINE)
	        call amovr (x, z, npts)
	    } else {
		call strcpy (Memc[IC_LABELS(ic,2)], Memc[label], SZ_LINE)
		call strcpy (Memc[IC_UNITS(ic,2)], Memc[units], SZ_LINE)
	        call amovr (y, z, npts)
	    }
	    call icg_uaxesr (axistype, cv, x, y, z, npts, Memc[label],
		Memc[units], SZ_LINE)
	    call gt_sets (gt, gtlabel[axis], Memc[label])
	    call gt_sets (gt, gtunits[axis], Memc[units])
	    call mfree (label, TY_CHAR)
	    call mfree (units, TY_CHAR)
	}
end


# ICG_DVZ -- Error action to take on zero division.

real procedure icg_dvzr (x)

real	x			# Numerator

begin
	return (1.)
end

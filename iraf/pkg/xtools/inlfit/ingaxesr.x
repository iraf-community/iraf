include	<pkg/gtools.h>
include	<pkg/inlfit.h>

# ING_AXES -- Set axes data. The applications program may set additional
# axes types.

procedure ing_axesr (in, gt, nl, axis, x, y, z, npts, nvars)

pointer	in				# INLFIT pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
int	axis				# Output axis
real	x[ARB]				# Independent variables (npts * nvars)
real	y[npts]				# Dependent variable
real	z[npts]				# Output values
int	npts				# Number of points
int	nvars				# Number of variables

int	i, j
int	axistype, axisnum
int	gtlabel[2], gtunits[2]
real 	a, b, xmin, xmax
pointer	sp, label, units, minptr, maxptr

real	nlevalr()
real	ing_dvzr()
errchk	adivr()
extern	ing_dvzr()

data	gtlabel/GTXLABEL, GTYLABEL/
data	gtunits/GTXUNITS, GTYUNITS/

int	in_geti()
pointer	in_getp()

begin
	# Allocate string space.
	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)

	# Get the appropiate axis type and variable number.
	call in_gkey (in, in_geti (in, INLGKEY), axis, axistype, axisnum)

	# Get and set axes labels and units.
	call ing_getlabel (in, axistype, axisnum, Memc[label], Memc[units],
	    SZ_LINE)
	call gt_sets (gt, gtlabel[axis], Memc[label])
	call gt_sets (gt, gtunits[axis], Memc[units])

	# Branch on axis type.
	switch (axistype) {
	case KEY_VARIABLE:	# Independent variable
	    do i = 1, npts
		z[i] = x[(i-1)*nvars+axisnum]
	case KEY_FUNCTION:	# Function variable
	    call amovr (y, z, npts)
	case KEY_FIT:		# Fitted values
	    call nlvectorr (nl, x, z, npts, nvars)
	case KEY_RESIDUALS:	# Residuals
	    call nlvectorr (nl, x, z, npts, nvars)
	    call asubr (y, z, z, npts)
	case KEY_RATIO:		# Ratio
	    call nlvectorr (nl, x, z, npts, nvars)
	    call advzr (y, z, z, npts, ing_dvzr)
	case KEY_NONLINEAR:	# Linear component removed
	    call aclrr (z, npts)
	    minptr = in_getp (in, INLXMIN)
	    maxptr = in_getp (in, INLXMAX)
	    a = nlevalr (nl, Memr[minptr], nvars)
	    do i = 1, nvars {
		xmin = Memr[minptr+i-1]
		xmax = Memr[maxptr+i-1]
		Memr[minptr+i-1] = xmax
		b = (nlevalr (nl, Memr[minptr], nvars) - a) /
		    (xmax - xmin)
		Memr[minptr+i-1] = xmin
		do j = 1, npts
		    z[j] = z[j] + y[j] - a - b * (x[(j-1)*nvars+i] - xmin)
	    }
	case KEY_UAXIS:	# User axes plots.
	    if (axis == 1) {
	        do i = 1, npts
		    z[i] = x[(i-1)*nvars+axisnum]
	    } else
	        call amovr (y, z, npts)
	    call ing_uaxesr (axisnum, in, nl, x, y, z, npts, nvars)
	default:
	    call error (0, "ing_axes: Unknown axis type")
	}

	# Free memory.
	call sfree (sp)
end


# ING_DVZ -- Error action to take on zero division.

real procedure ing_dvzr (x)

real	x			# Numerator

begin
	return (real (1.0))
end

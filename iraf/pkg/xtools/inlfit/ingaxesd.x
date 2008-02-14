include	<pkg/gtools.h>
include	<pkg/inlfit.h>

# ING_AXES -- Set axes data. The applications program may set additional
# axes types.

procedure ing_axesd (in, gt, nl, axis, x, y, z, npts, nvars)

pointer	in				# INLFIT pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
int	axis				# Output axis
double	x[ARB]				# Independent variables (npts * nvars)
double	y[npts]				# Dependent variable
double	z[npts]				# Output values
int	npts				# Number of points
int	nvars				# Number of variables

int	i, j
int	axistype, axisnum
int	gtlabel[2], gtunits[2]
double 	a, b, xmin, xmax
pointer	sp, label, units, minptr, maxptr

double	nlevald()
double	ing_dvzd()
errchk	adivd()
extern	ing_dvzd()

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
	    call amovd (y, z, npts)
	case KEY_FIT:		# Fitted values
	    call nlvectord (nl, x, z, npts, nvars)
	case KEY_RESIDUALS:	# Residuals
	    call nlvectord (nl, x, z, npts, nvars)
	    call asubd (y, z, z, npts)
	case KEY_RATIO:		# Ratio
	    call nlvectord (nl, x, z, npts, nvars)
	    call advzd (y, z, z, npts, ing_dvzd)
	case KEY_NONLINEAR:	# Linear component removed
	    call aclrd (z, npts)
	    minptr = in_getp (in, INLXMIN)
	    maxptr = in_getp (in, INLXMAX)
	    a = nlevald (nl, Memd[minptr], nvars)
	    do i = 1, nvars {
		xmin = Memd[minptr+i-1]
		xmax = Memd[maxptr+i-1]
		Memd[minptr+i-1] = xmax
		b = (nlevald (nl, Memd[minptr], nvars) - a) /
		    (xmax - xmin)
		Memd[minptr+i-1] = xmin
		do j = 1, npts
		    z[j] = z[j] + y[j] - a - b * (x[(j-1)*nvars+i] - xmin)
	    }
	case KEY_UAXIS:	# User axes plots.
	    if (axis == 1) {
	        do i = 1, npts
		    z[i] = x[(i-1)*nvars+axisnum]
	    } else
	        call amovd (y, z, npts)
	    call ing_uaxesd (axisnum, in, nl, x, y, z, npts, nvars)
	default:
	    call error (0, "ing_axes: Unknown axis type")
	}

	# Free memory.
	call sfree (sp)
end


# ING_DVZ -- Error action to take on zero division.

double procedure ing_dvzd (x)

double	x			# Numerator

begin
	return (double (1.0))
end

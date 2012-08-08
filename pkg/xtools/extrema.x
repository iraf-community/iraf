# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	ORDER		4	# The order of the spline

# EXTREMA -- Find the extrema in an array of x and y points.
# The input data points are fitted with a cubic interpolation spline.  The
# spline is then searched for points where the first derivative changes sign.
# The minimum step size of this search is controlled by the parameter dx.
# The positions of these extrema are returned in the x array, the value of the
# spline at the extrema are returned in the y array, and the curvature or
# second derivative of the spline at the extrema are returned in the
# curvature array.  The function returns the number of extrema found.

int procedure extrema (x, y, curvature, npts, dx)

real	x[npts], y[npts]	# Input data points and output extrema
real	curvature[npts]		# 2nd deriv. of cubic spline at extrema
int	npts			# Number of input data points
real	dx			# Precision of extrema positions

int	i, ier, nextrema
real	xeval, left_deriv, right_deriv
pointer	sp, bspln, q
real	seval()
errchk	salloc, seval

begin
	# Allocate working arrays for spline routines
	call smark (sp)
	call salloc (bspln, 2 * npts + 30, TY_REAL)
	call salloc (q, (2 * ORDER - 1) * npts, TY_REAL)

	# Calculate the spline coefficients
	call spline (x, y, npts, Memr[bspln], Memr[q], ORDER, ier)
	if (ier != 0) {
	    call sfree (sp)
	    return (0)
	}

	# Initialize the curvature array
	call aclrr (curvature, npts)

	# Find the extrema defined by a change in sign in the first derivative.
	nextrema = 0
	left_deriv = seval (x[1], 1, Memr[bspln])
	do i = 2, npts {
	    xeval = x[i]
	    right_deriv = seval (xeval, 1, Memr[bspln])
	    if (left_deriv * right_deriv <= 0.) {
		for (xeval = x[i - 1] + dx; xeval <= x[i]; xeval = xeval + dx) {
		    right_deriv = seval (xeval, 1, Memr[bspln])
	    	    if (left_deriv * right_deriv <= 0.)
			break
		    left_deriv = right_deriv
		}
		nextrema = nextrema + 1
		x[nextrema] = xeval
		y[nextrema] = seval (xeval, 0, Memr[bspln])
		curvature[nextrema] = seval (xeval, 2, Memr[bspln])
		if (curvature[nextrema] == 0.)
		    nextrema = nextrema - 1
		if (nextrema == npts)
		    break
	    }
	    left_deriv = right_deriv
	}

	call sfree (sp)
	return (nextrema)
end

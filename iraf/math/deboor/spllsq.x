# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"bspln.h"

.help spllsq 2 "math library"
.ih ___________________________________________________________________________
NAME
spllsq -- general smoothing b-spline with uniform knots.
.ih
USAGE
spllsq (x, y, w, npts, bspln, q, work, k, n, wflg, ier)
.ih
PARAMETERS
See the listing of SPLSQV if a more detailed discussion of the parameters
than that given here is desired.
.ls x,y,w
(real[npts]).  Abscissae, ordinates, and weights of the data points.
X[1] and X[NPTS] determine the range of the spline (the interval over which
it can be evaluated).  Dummy data points with zero weight can be supplied
to fix the range if desired.
.le
.ls npts
The number of data points.
.le
.ls bspln
(real[2*n+30]).  Output array of N b-spline coefficients, N+K knots,
and the spline descriptor structure.
.le
.ls q
(real[n,k]).  Work array.
.le
.ls work
(real[n]).  Work array.
.le
.ls k
The order of the desired spline (1-20) (Cubic == 4).
.le
.ls n
N is the number of b-spline coefficients to be output.  The relationship
between N and the number of polynomial pieces in the spline (NPP)
is given by NPP = N-K-1.  Note that NPP is the important parameter.
N is used in the parameter list rather than NPP because it is used in
dimensioning the arrays.
.le
.ls wflg
Weight generation flag.  Options:

.nf
    0  pass W on to SPLSQV as is
    1  calculate default weights
    2  same as 1, but set W[i] only
         if W[i] already NONZERO
.fi

If WFLG==2, data points may be rejected from the fit by setting their
corresponding weight to zero, before calling SPLLSQ (good points must be
assigned nonzero ws before calling SPLLSQ).  Use WFLG==1 if no points
are to be rejected, to avoid having to initialize the W array on every
call.
.le
.ls ier
Error return.  Zero if no error.  Nonzero indicates invalid combination
of N, K, and NPTS.
.le
.ih
DESCRIPTION
A general algorithm for smoothing with uniform B-splines of arbitrary order.
Calls SPLSQV, which is adapted from L2APPR, as described in "A Practical
Guide To Splines", by C. DeBoor.
 
SPLLSQ is identical to SPLSQV, except that (1): the array T[n+k], giving
the x-positions of the knots of the b-spline, is generated internally,
rather than by the calling program, and (2): the weights associated with
the data points may be calculated automatically.  The knots are generated
with a uniform spacing.  Note that the x-values of the data points do not
have to be uniformly spaced, but they must be monotonically increasing, and
(1) both must span the same range, and (2) if N spline coefficients are
desired, there must be at least N+K data points.
.ih
BUGS
A routine FSPLSQ needs to be written to make use of the Cholesky factorization
returned by SPLSQV in W1, for more efficient fitting of subsequent data sets
that differ only in the y-values of the data points.
.ih
SEE ALSO
seval(2)
.endhelp _______________________________________________________________________


procedure spllsq (x, y, w, npts, bspln, q, work, k, n, wflg, ier)

real	x[npts], y[npts], w[npts]
real	q[n,k], work[n]
real	bspln[ARB]
int	wflg, npts, n, k, ier
int	i, npp, km1, knot
real	dx

begin
	ier = 0				#successful return value
	npp = n - k + 1			#number of polynomial pieces
	if (npp <= 0) {		
	    ier = 1
	    return
	}				

	# Set up spline descriptor in the array BCOEF, for later evaluation
	# of the spline by BSPLN (see "bcoef.h" for definitions of the fields).

	NCOEF = n				#number of b-spline coeff
	ORDER = k				#order of the spline
	XMIN = x[1]				#x at left endpoint
	XMAX = x[npts]				#x at right endpoint
	KINDEX = KNOT1 - 1 + k			#for SEVAL


	# Set values of knots.  First K knots take on the value of the first
	# breakpoint, next npp-1 knots have spacing DX, and the last K knots
	# take on the value of the last breakpoint.

	dx = (XMAX - XMIN ) / npp		#span(x) = span(t)

	km1 = k - 1
	knot = KNOT1 - 1

	do i = 1, km1
	    bspln[knot+i] = XMIN

	knot = knot + km1
	do i = 1, npp
	    bspln[knot+i] = XMIN + (i-1) * dx

	knot = knot + npp
	do i = 1, k
	    bspln[knot+i] = XMAX


	# Calculate default weights.  The default weight of a datapoint is
	# proportional to the spacing.

	if (wflg > 0) {
	    if (w[1] > 0  ||  wflg == 1)
		w[1] = x[2] - x[1]

	    do i = 2, npts - 1 {
		if (w[i] > 0  ||  wflg == 1)
		    w[i] = (x[i+1] - x[i-1]) / 2.
	    }
	    if (w[npts] > 0  ||  wflg == 1)
		w[npts] = x[npts] - x[npts-1]
	}


	# Call SPLSQV to do the actual spline fit.  The N b-spline coefficients
	# of order K, N+K knots, and the spline descriptor are returned in
	# BSPLN, for evaluation of the spline via SEVAL.

	call splsqv (x, y, w, npts, bspln[nint(KNOT1)], n, k, q,
	    work, bspln[COEF1], ier)
end

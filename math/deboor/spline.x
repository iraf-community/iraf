# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


include	"bspln.h"

.help spline 2 "math library"
.ih
NAME
spline -- generalized spline interpolation with b-splines.
.ih
USAGE
spline (x, y, n, bspln, q, k, ier)
.ih
PARAMETERS
.ls x,y
(real[n]).  Abcissae and ordinates of the N data points.
.le
.ls n
The number of input data points, and the number of spline coefficients
to be generated.
.le
.ls bspln
(real[2*n+30]).  Output array of N b-spline coefficients, N+K knots,
and the spline descriptor array.
.le
.ls q
(real[(2*K-1)*N]).  On output, will contain the
triangular factorization of the coefficient matrix of the linear
system for the b-coefficients of the spline interpolant.  If a new
data set differs only in the Y values, Q may be input to FSPLIN to
efficiently solve for the b-spline coefficients of the new data set.
.le
.ls k
The order of the spline (2-20).  Must be EVEN at present
(k=4 is the cubic spline).
.le
.ls ier
Zero if ok, nonzero if error (invalid input parameters).
.le
.ih
DESCRIPTION
General spline interpolation.  SPLINE is a thinly disguised version of SPLINT
(DeBoor, pg. 204).  SPLINE differs from SPLINT in that the knot spacing T is
calculated automatically, using the not-a-knot boundary conditions.  Only even
K are permitted at present (K = 2, 4, 6,...).  The cubic spline interpolant
corresponds to K = 4.  SPLINE saves a complete description of the spline in the
BSPLN array, for use later by SEVAL (used to evaluate the fitted spline) and
FSPLIN (used after a call to SPLINE to more efficiently fit subsequent data
sets).
.ih
SOURCE
See the listing of SPLINT, or Chapter XIII of DeBoors book.
.ih
SEE ALSO
seval(2), fsplin(2).
.endhelp ______________________________________________________________


procedure spline (x, y, n, bspln, q, k, ier)

real	x[n], y[n]
int	n, k, ier
real	q[ARB], bspln[ARB]		#q[(2*k-1)*n], bspln[2*n+30]
int	m, i, knot

begin
	if (k < 2 || mod (k, 2) != 0) {
	    ier = 1
	    return
	}

	NCOEF = n			#set up spline descriptor
	ORDER = k
	XMIN = x[1]
	XMAX = x[n]

	knot = int (KNOT1) - 1		#offset to knot array in bspln
	KINDEX = knot + k		#initial posn for SEVAL

	m = knot + n
	do i = 1, k {			#not-a-knot knot boundary conditions
	    bspln[knot+i] = XMIN
	    bspln[m+i] = XMAX
	}

	m = k / 2			#use data x-values inside
	do i = k+1, n
	    bspln[knot+i] = x[i+m-k]

	call splint (x, y, bspln[knot+1], n, k, q, bspln[COEF1], ier)
	if (ier == 1)			#1=success, 2==failure
	    ier = 0
end

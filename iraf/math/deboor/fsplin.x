# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


include	"bspln.h"

.help fsplin	2	"math library"
.ih
NAME
fsplin -- fast fit of b-spline interpolant.
.ih
USAGE
fsplin (y, q, bspln)
.ih
PARAMETERS
.ls y
(real[n]).  Array of y-values of new data set.
.le
.ls q
(real[(2*k-1)*n]).  Array containing the triangular factorization of the
coefficient matrix of the linear system for the b-spline coefficients of
the spline interpolant of dimension N and order K.  Q is produced by a
prior call to SPLINE.
.le
.ls bspln
(real[2*n+30]).  The spline descriptor array.  On input to FSPLIN, should
contain a valid spline header (containing N, K, etc.), and knot array.
As long as SPLINE is called before FSPLIN, the bspln array will be set up
properly.  On output, the N b-spline coefficients are stored in BSPLN,
ready for immediate input to SEVAL to evaluate the spline.
.le
.ih
DESCRIPTION
Fsplin is used following an initial SPLINE to efficiently fit a Kth order
b-spline to a data set that differs from the data set input to SPLINE only
in the y-values of the data points.  SPLINE and FSPLIN are used to
interpolate an arbitrary array of data points (x,y), by fitting a piecewise
Kth order curve, continuous in the first K-1 derivatives, through the
data points.
.ih
SOURCE
See Carl De Boor, "A Practical Guide to Splines", pg. 204-207.
.ih
SEE ALSO
spline(2), seval(2)
.endhelp ______________________________________________________________


procedure fsplin (y, q, bspln)

real	y[ARB], q[ARB]
real	bspln[ARB]
int	n, k, offset, i

begin
	offset = COEF1 - 1			#copy data into COEF array
	do i = 1, NCOEF
	    bspln[offset+i] = y[i]

	n = NCOEF
	k = ORDER

	call banslv (q, 2*k - 1, n, k-1, k-1, bspln[offset+1])
end

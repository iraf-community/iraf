# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"

# II_POLTERP -- polynomial interpolator with x and y arrays given.
# This algorithm is based on the Newton form as described in
# C. de Boor's book, "A Practical Guide to Splines", 1978.
# There is no error checking - this is meant to be used only by calls
# from more complete routines that take care of such things.

# Maximum number of terms is MAX_NDERIVS.

real procedure ii_polterp (x, y, n, x0)

real	x[ARB],y[ARB]	# x and y array
real	x0		# desired x
int	n		# number of points in x and y = number of
			# terms in polynomial = order + 1

int	k,i
real	d[MAX_NDERIVS]

begin

	# Fill in entries for divided difference table.
	do i = 1, n
	    d[i] = y[i]

	# Generate divided differences
	do k = 1, n - 1
	    do i = 1, n - k
		d[i] = (d[i+1] - d[i])/(x[i+k] - x[i])

	# Shift divided difference table to center on x0
	do i = 2, n
	    d[i] = d[i] + d[i-1] * (x0 - x[i])

	return (d[n])
end

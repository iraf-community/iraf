# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# FP_EQUALR -- The following procedure is used to compare two single precision
# numbers for equality to within the machine precision for doubles.  A simple
# comparison of the difference of the two numbers with the machine epsilon
# does not suffice unless the numbers are first normalized to near 1.0, the
# constant used to compute the machine epsilon (epsilon is the smallest number
# such that 1.0 + epsilon > 1.0).

bool procedure fp_equalr (x, y)

real	x, y
real	x1, x2, normx, normy, tol
int	ex, ey

begin
	tol = EPSILONR * 10.0
	if (x == y)
	    return (true)

	call fp_normr (x, normx, ex)
	call fp_normr (y, normy, ey)

	if (ex != ey)
	    return (false)
	else {
	    x1 = 1.0E0 + abs (normx - normy)
	    x2 = 1.0E0 + tol
	    return (x1 <= x2)
	}
end

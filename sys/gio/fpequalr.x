# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# FP_EQUALR -- The following procedure is used to compare two single precision
# numbers for equality to within the machine precision for single.  A simple
# comparison of the difference of the two numbers with the machine epsilon
# does not suffice unless the numbers are first normalized to near 1.0, the
# constant used to compute the machine epsilon (epsilon is the smallest number
# such that 1.0 + epsilon > 1.0).

bool procedure fp_equalr (x, y)

real	x, y
real	x1, x2, normx, normy, tol
int	ex, ey

begin
	# Check for the obvious first.
	if (x == y)
	    return (true)

	# We can't normalize zero, so handle the zero operand cases first.
	# Note that the case 0 equals 0 is handled above.

	if (x == 0.0D0 || y == 0.0D0)
	    return (false)

	# Normalize operands and do an epsilon compare.
	call fp_normr (x, normx, ex)
	call fp_normr (y, normy, ey)

	if (ex != ey)
	    return (false)
	else {
	    tol = EPSILONR * 32.0
	    x1 = 1.0E0 + abs (normx - normy)
	    x2 = 1.0E0 + tol
	    return (x1 <= x2)
	}
end

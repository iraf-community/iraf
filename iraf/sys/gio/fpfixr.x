# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# FP_FIXR -- The following procedure is equivalent to "int(x)", except that
# it preserves the most significant digits of x, when x is greater than the
# largest integer.  For example, if an integer is 32 bits and X has a 58 bit
# mantissa, "int(x)" would cause nearly half the precision to be lost.
# 
# Algorithm (x is assumed nonnegative):
#    (1) find high, low x such that  x = highx + lowx
#	and highx contains the extra digits of precision.
#    (2) subtract highx from x, and truncate the residual by assignment
#	into a long integer.
#    (3) add truncated lowx and highx to get high precision truncated
#	real or double result.

real procedure fp_fixr (x)

real	x
real	absx, highx, scaledx
int	expon
long	longx, lowx

begin
	absx = abs (x)
	scaledx = absx
	expon = 0

	while (scaledx > MAX_LONG) {
	    scaledx = scaledx / 10.0E0
	    expon = expon + 1
	}

	longx = scaledx
	highx = longx * (10.0E0 ** expon)
	lowx = absx - highx

	if (x > 0)
	    return (highx + lowx)
	else
	    return (-highx - lowx)
end

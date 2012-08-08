# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# FP_NORMR -- Normalize a single precision number x to the value NORMX, in the
# range [1-10).  EXPON is returned such that  x = normx * (10.0E0 ** expon).

procedure fp_normr (x, normx, expon)

real	x			# number to be normalized
real	normx			# X normalized to the range 1-10 (output)
int	expon			# exponent of normalized X
real	absx, tol

begin
	tol = EPSILONR * 10.0
	absx = abs (x)
	expon = 0

	if (absx > 0) {
	    while (absx < (1.0E0 - tol)) {
		absx = absx * 10.0E0
		expon = expon - 1
		if (absx == 0.0) {	# check for underflow to zero
		    normx = 0
		    expon = 0
		    return
		}
	    }
	    while (absx >= (10.0E0 + tol)) {
		absx = absx / 10.0E0
		expon = expon + 1
	    }
	}

	if (x < 0)
	    normx = -absx
	else
	    normx = absx
end

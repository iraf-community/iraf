# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# FP_NORMD -- Normalize a double precision number x to the value NORMX, in the
# range [1-10).  EXPON is returned such that  x = normx * (10.0d0 ** expon).

procedure fp_normd (x, normx, expon)

double	x			# number to be normalized
double	normx			# X normalized to the range 1-10 (output)
int	expon			# exponent of normalized X
double	absx, tol

begin
	tol = EPSILOND * 10.0D0
	absx = abs (x)
	expon = 0

	if (absx > 0) {
	    while (absx < (1.0D0 - tol)) {
		absx = absx * 10.0D0
		expon = expon - 1
		if (absx == 0.0D0) {	# check for underflow to zero
		    normx = 0
		    expon = 0
		    return
		}
	    }
	    while (absx >= (10.0D0 + tol)) {
		absx = absx / 10.0D0
		expon = expon + 1
	    }
	}

	if (x < 0)
	    normx = -absx
	else
	    normx = absx
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>


# DTCSCL -- Scales a double precision real, maintaining maximum precision.
# Called by DTOC and CTOD.

procedure dtcscl (v, e, sense)

double	v		# value to be scaled
int	e		# exponent
int	sense		# sense of scaling (0=apply e to v;  1=calc e)

bool	dt_equald()

begin
	if (sense == 0) 		# scale v by 10 ** e
	    v = v * (10.0d0 ** e)

	else {				# scale number to 0.1 <= v < 1.0
	    if (dt_equald (v, 0.0d0))
		e = 0
	    else {
	        e = -1
	        while (v < 0.1d0) {
		    v = v * 10.0d0
		    e = e - 1
		    if (e <= (- MAX_EXPONENTD) || dt_equald (v, 0.0d0)) {
			e = 0
			break
		    }
	        }
	        while (v >= 1.0d0) {
		    v = v / 10.0d0
		    e = e + 1
		    if (e >= MAX_EXPONENTD)
		        break
	        }
	    }
	}
end


# DT_EQUALD -- The following procedure is used to compare two double precision
# numbers for equality to within the machine precision for doubles.  A simple
# comparison of the difference of the two numbers with the machine epsilon
# does not suffice unless the numbers are first normalized to near 1.0, the
# constant used to compute the machine epsilon (epsilon is the smallest number
# such that 1.0 + epsilon > 1.0).

bool procedure dt_equald (x, y)

double	x, y
double	x1, x2, normx, normy, tol
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
	call dt_normd (x, normx, ex)
	call dt_normd (y, normy, ey)

	if (ex != ey)
	    return (false)
	else {
	    tol = EPSILOND * 32.0D0
	    x1 = 1.0D0 + abs (normx - normy)
	    x2 = 1.0D0 + tol
	    return (x1 <= x2)
	}
end


# DT_NORMD -- Normalize a double precision number x to the value NORMX, in the
# range [1-10].  EXPON is returned such that  x = normx * (10.0d0 ** expon).

procedure dt_normd (x, normx, expon)

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

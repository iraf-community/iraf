# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARCZ -- Vector reciprocal with checking for zero divisors.  If the result
# of a divide would be undefined a user supplied function is called to get the
# output pixel value.
#
# NOTE: in the interests of simplicity a somewhat arbitrary tolerance is used
# to check for an undefined divide, i.e., a divide by zero or a divide by a
# number small enough to cause floating point overflow.  A better way to do
# this would be to provide a machine dependent version of this operator in
# host$as which catches the hardware exception rather than using a comparison.

procedure arczd (a, b, c, npix, errfcn)

double	a			# numerator
double	b[ARB], c[ARB]		# divisor, and output arrays
int	npix			# number of pixels
double	errfcn()		# user function, called on divide by zero

int	i
double	divisor
double	tol
extern	errfcn()
errchk	errfcn

begin
	if (a == 0.0D0) {
	    call aclrd (c, npix)
	    return
	}

	    tol = 1.0D-20

	do i = 1, npix {
	    divisor = b[i]
		# The following is most efficient when the data tends to be
		# positive.

		if (divisor < tol)
		    if (divisor > -tol) {
			c[i] = errfcn (a)
			next
		    }
		c[i] = a / divisor

	}
end

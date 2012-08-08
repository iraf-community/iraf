# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ADVZ -- Vector divide with checking for zero divisors.  If the result of a
# divide would be undefined a user supplied function is called to get the
# output pixel value.
#
# NOTE: in the interests of simplicity a somewhat arbitrary tolerance is used
# to check for an undefined divide, i.e., a divide by zero or a divide by a
# number small enough to cause floating point overflow.  A better way to do
# this would be to provide a machine dependent version of this operator in
# host$as which catches the hardware exception rather than using a comparison.

procedure advzr (a, b, c, npix, errfcn)

real	a[ARB], b[ARB], c[ARB]	# numerator, divisor, and output arrays
int	npix			# number of pixels
real	errfcn()		# user function, called on divide by zero

int	i
real	divisor
real	tol
extern	errfcn()
errchk	errfcn

begin
	    tol = 1.0E-20

	do i = 1, npix {
	    divisor = b[i]
		# The following is most efficient when the data tends to be
		# positive.

		if (divisor < tol)
		    if (divisor > -tol) {
			c[i] = errfcn (a[i])
			next
		    }
		c[i] = a[i] / divisor

	}
end

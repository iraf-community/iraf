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

procedure advzx (a, b, c, npix, errfcn)

complex	a[ARB], b[ARB], c[ARB]	# numerator, divisor, and output arrays
int	npix			# number of pixels
complex	errfcn()		# user function, called on divide by zero

int	i
complex	divisor
extern	errfcn()
errchk	errfcn

begin

	do i = 1, npix {
	    divisor = b[i]
		if (divisor == (0.0,0.0))
		    c[i] = errfcn (a[i])
		else
		    c[i] = a[i] / divisor
	}
end

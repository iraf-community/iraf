# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARCP -- Reciprocal of a constant divided by a vector.  No divide by zero
# checking is performed.

procedure arcpr (a, b, c, npix)

real	a		# constant numerator
real	b[ARB]		# vector denominator
real	c[ARB]		# output vector
int	npix
int	i

begin
	if (a == 0.0) {
	    call aclrr (c, npix)
	} else if (a == 1.0) {
	    do i = 1, npix
		c[i] = 1.0 / b[i]
	} else {
	    do i = 1, npix
		c[i] = a / b[i]
	}
end

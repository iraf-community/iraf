# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARCP -- Reciprocal of a constant divided by a vector.  No divide by zero
# checking is performed.

procedure arcpx (a, b, c, npix)

complex	a		# constant numerator
complex	b[ARB]		# vector denominator
complex	c[ARB]		# output vector
int	npix
int	i

begin
	if (a == (0.0,0.0)) {
	    call aclrx (c, npix)
	} else if (a == (1.0,1.0)) {
	    do i = 1, npix
		c[i] = (1.0,1.0) / b[i]
	} else {
	    do i = 1, npix
		c[i] = a / b[i]
	}
end

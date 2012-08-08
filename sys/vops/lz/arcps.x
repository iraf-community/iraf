# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARCP -- Reciprocal of a constant divided by a vector.  No divide by zero
# checking is performed.

procedure arcps (a, b, c, npix)

short	a		# constant numerator
short	b[ARB]		# vector denominator
short	c[ARB]		# output vector
int	npix
int	i

begin
	if (a == 0) {
	    call aclrs (c, npix)
	} else if (a == 1) {
	    do i = 1, npix
		c[i] = 1 / b[i]
	} else {
	    do i = 1, npix
		c[i] = a / b[i]
	}
end

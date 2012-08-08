# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARCP -- Reciprocal of a constant divided by a vector.  No divide by zero
# checking is performed.

procedure arcpd (a, b, c, npix)

double	a		# constant numerator
double	b[ARB]		# vector denominator
double	c[ARB]		# output vector
int	npix
int	i

begin
	if (a == 0.0D0) {
	    call aclrd (c, npix)
	} else if (a == 1.0D0) {
	    do i = 1, npix
		c[i] = 1.0D0 / b[i]
	} else {
	    do i = 1, npix
		c[i] = a / b[i]
	}
end

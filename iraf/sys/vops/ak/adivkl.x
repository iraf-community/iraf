# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ADIVK -- Divide a vector by a constant (generic).  No divide by zero checking
# is performed.

procedure adivkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] / b
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMULK -- Multiply a constant times a vector (generic).

procedure amulkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] * b
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMULK -- Multiply a constant times a vector (generic).

procedure amulkr (a, b, c, npix)

real	a[ARB]
real	b
real	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] * b
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AEXPK -- Compute a ** b, where b is a constant of type PIXEL (generic).

procedure aexpkx (a, b, c, npix)

complex	a[ARB]
complex	b
complex	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b
end

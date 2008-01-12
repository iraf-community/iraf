# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AEXPK -- Compute a ** b, where b is a constant of type PIXEL (generic).

procedure aexpkr (a, b, c, npix)

real	a[ARB]
real	b
real	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AEXPK -- Compute a ** b, where b is a constant of type PIXEL (generic).

procedure aexpkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b
end

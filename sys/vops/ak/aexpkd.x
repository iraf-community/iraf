# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AEXPK -- Compute a ** b, where b is a constant of type PIXEL (generic).

procedure aexpkd (a, b, c, npix)

double	a[ARB]
double	b
double	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b
end

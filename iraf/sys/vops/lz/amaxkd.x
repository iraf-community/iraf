# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxkd (a, b, c, npix)

double	a[ARB]
double	b
double	c[ARB]
size_t	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

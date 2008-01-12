# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminkd (a, b, c, npix)

double	a[ARB]
double	b
double	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = min (a[i], b)
end

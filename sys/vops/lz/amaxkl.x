# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxkr (a, b, c, npix)

real	a[ARB]
real	b
real	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

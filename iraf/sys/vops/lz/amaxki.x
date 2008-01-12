# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = min (a[i], b)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAXK -- Compute the maximum of a constant and a vector (generic).

procedure amaxkc (a, b, c, npix)

char	a[ARB]
char	b
char	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = max (a[i], b)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminkc (a, b, c, npix)

char	a[ARB]
char	b
char	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = min (a[i], b)
end

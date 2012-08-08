# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin

	do i = 1, npix
		c[i] = min (a[i], b)
end

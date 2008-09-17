# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminkp (a, b, c, npix)

pointer	a[ARB]
pointer	b
pointer	c[ARB]
size_t	npix, i

begin

	do i = 1, npix
		c[i] = min (a[i], b)
end

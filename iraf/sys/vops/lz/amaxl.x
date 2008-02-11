# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAX -- Compute the maximum of two vectors (generic).

procedure amaxl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
size_t	npix, i

begin
	do i = 1, npix
		c[i] = max (a[i], b[i])
end

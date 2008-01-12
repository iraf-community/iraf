# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAG -- Return the magnitude of two vectors.

procedure amags (a, b, c, npix)

short	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		c[i] = sqrt (real(a[i] ** 2) + real(b[i] ** 2))
end

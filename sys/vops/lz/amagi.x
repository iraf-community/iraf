# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAG -- Return the magnitude of two vectors.

procedure amagi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		c[i] = sqrt (real(a[i] ** 2) + real(b[i] ** 2))
end

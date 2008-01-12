# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAG -- Return the magnitude of two vectors.

procedure amagd (a, b, c, npix)

double	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		c[i] = sqrt (double(a[i] ** 2) + double(b[i] ** 2))
end

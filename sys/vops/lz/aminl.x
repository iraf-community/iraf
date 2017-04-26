# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMIN -- Compute the minimum of two vectors (generic).

procedure aminl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		c[i] = min (a[i], b[i])
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAX -- Compute the maximum of two vectors (generic).

procedure amaxi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		c[i] = max (a[i], b[i])
end

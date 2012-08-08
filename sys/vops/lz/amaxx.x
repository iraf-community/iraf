# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAX -- Compute the maximum of two vectors (generic).

procedure amaxx (a, b, c, npix)

complex	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		if (abs(a[i]) >= abs(b[i]))
		    c[i] = a[i]
		else
		    c[i] = b[i]
end

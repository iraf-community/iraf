# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOD -- Compute the modulus of two vectors (generic).

procedure amodl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = mod (a[i], b[i])
end

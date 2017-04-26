# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMODK -- Compute the modulus of a vector by a constant (generic).

procedure amodkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = mod (a[i], b)
end

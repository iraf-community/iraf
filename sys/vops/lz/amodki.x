# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMODK -- Compute the modulus of a vector by a constant (generic).

procedure amodki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = mod (a[i], b)
end

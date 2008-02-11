# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMODK -- Compute the modulus of a vector by a constant (generic).

procedure amodki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
size_t	npix, i
int	modi()

begin
	do i = 1, npix
	    c[i] = modi (a[i], b)
end

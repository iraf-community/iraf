# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMODK -- Compute the modulus of a vector by a constant (generic).

procedure amodks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
size_t	npix, i
short	mods()

begin
	do i = 1, npix
	    c[i] = mods (a[i], b)
end

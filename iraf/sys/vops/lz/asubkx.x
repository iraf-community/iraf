# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUBK -- Subtract a constant from a vector (generic).

procedure asubkx (a, b, c, npix)

complex	a[ARB]
complex	b
complex	c[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] - b
end

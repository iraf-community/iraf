# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AADDK -- Add a constant to a vector (generic).

procedure aaddkp (a, b, c, npix)

pointer	a[ARB]
pointer	b
pointer	c[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] + b
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMULK -- Multiply a constant times a vector (generic).

procedure amulkp (a, b, c, npix)

pointer	a[ARB]
pointer	b
pointer	c[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] * b
end

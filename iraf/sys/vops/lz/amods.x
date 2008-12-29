# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOD -- Compute the modulus of two vectors (generic).

procedure amods (a, b, c, npix)

short	a[ARB], b[ARB], c[ARB]
size_t	npix, i
short	smod()

begin
	do i = 1, npix
	    c[i] = smod (a[i], b[i])
end

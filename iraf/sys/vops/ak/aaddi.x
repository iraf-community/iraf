# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AADD -- Add two vectors (generic).

procedure aaddi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] + b[i]
end

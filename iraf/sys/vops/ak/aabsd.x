# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AABS -- Compute the absolute value of a vector (generic).

procedure aabsd (a, b, npix)

double	a[ARB], b[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    b[i] = abs(a[i])
end

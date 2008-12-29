# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AABS -- Compute the absolute value of a vector (generic).

procedure aabsi (a, b, npix)

int	a[ARB], b[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    b[i] = iabs(a[i])
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AABS -- Compute the absolute value of a vector (generic).

procedure aabss (a, b, npix)

short	a[ARB], b[ARB]
size_t	npix, i
short	sabs()

begin
	do i = 1, npix
	    b[i] = sabs(a[i])
end

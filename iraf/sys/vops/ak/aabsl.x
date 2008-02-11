# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AABS -- Compute the absolute value of a vector (generic).

procedure aabsl (a, b, npix)

long	a[ARB], b[ARB]
size_t	npix, i
long	absl()

begin
	do i = 1, npix
	    b[i] = absl(a[i])
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALTM -- Linearly map a vector into another vector of the same datatype.
#	b[i] = (a[i] * k1) + k2

procedure altmr (a, b, npix, k1, k2)

real	a[ARB], b[ARB]
real	k1, k2
int	npix, i

begin
	do i = 1, npix
	    b[i] = (a[i] * k1) + k2
end

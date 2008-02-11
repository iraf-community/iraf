# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACLR -- Zero a vector (generic).

procedure aclrl (a, npix)

long	a[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    a[i] = 0
end

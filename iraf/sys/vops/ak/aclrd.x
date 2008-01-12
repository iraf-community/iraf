# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACLR -- Zero a vector (generic).

procedure aclrd (a, npix)

double	a[ARB]
int	npix, i

begin
	do i = 1, npix
	    a[i] = 0.0D0
end

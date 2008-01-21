# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACLR -- Zero a vector (generic).

procedure aclrp (a, npix)

pointer	a[ARB]
int	npix, i

begin
	do i = 1, npix
	    a[i] = 0
end

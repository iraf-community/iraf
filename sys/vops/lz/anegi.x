# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ANEG -- Compute the arithmetic negation of a vector (generic).

procedure anegi (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i

begin
	do i = 1, npix
	    b[i] = -a[i]
end

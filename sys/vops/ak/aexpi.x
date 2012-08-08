# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AEXP -- Compute a ** b, where b is of type PIXEL (generic).

procedure aexpi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b[i]
end

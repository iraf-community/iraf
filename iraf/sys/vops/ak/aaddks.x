# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AADDK -- Add a constant to a vector (generic).

procedure aaddks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] + b
end

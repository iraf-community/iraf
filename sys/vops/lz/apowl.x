# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# APOW -- Compute a ** b, where b is of type INT (generic).

procedure apowl (a, b, c, npix)

long	a[ARB], c[ARB]
int	b[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] ** b[i]
end

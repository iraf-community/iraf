# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUBK -- Subtract a constant from a vector (generic).

procedure asubkd (a, b, c, npix)

double	a[ARB]
double	b
double	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] - b
end

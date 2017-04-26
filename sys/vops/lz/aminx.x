# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMIN -- Compute the minimum of two vectors (generic).

procedure aminx (a, b, c, npix)

complex	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
		if (abs(a[i]) <= abs(b[i]))
		    c[i] = a[i]
		else
		    c[i] = b[i]
end

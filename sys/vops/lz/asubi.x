# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUB -- Subtract two vectors (generic).

procedure asubi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] - b[i]
end

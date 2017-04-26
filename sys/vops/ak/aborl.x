# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABOR -- Compute the bitwise boolean 'or' of two vectors (generic).

procedure aborl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
int	npix, i
long	orl()

begin
	do i = 1, npix {
		c[i] = orl (a[i], b[i])
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABOR -- Compute the bitwise boolean 'or' of two vectors (generic).

procedure abors (a, b, c, npix)

short	a[ARB], b[ARB], c[ARB]
int	npix, i
short	ors()

begin
	do i = 1, npix {
		c[i] = ors (a[i], b[i])
	}
end

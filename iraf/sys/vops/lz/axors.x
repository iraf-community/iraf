# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXOR -- Compute the exclusive or of two vectors (generic).

procedure axors (a, b, c, npix)

short	a[ARB], b[ARB], c[ARB]
int	npix, i
short	xors()

begin
	do i = 1, npix {
		c[i] = xors (a[i], b[i])
	}
end

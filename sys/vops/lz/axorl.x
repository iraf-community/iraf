# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXOR -- Compute the exclusive or of two vectors (generic).

procedure axorl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
int	npix, i
long	xorl()

begin
	do i = 1, npix {
		c[i] = xorl (a[i], b[i])
	}
end

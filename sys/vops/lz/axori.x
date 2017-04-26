# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXOR -- Compute the exclusive or of two vectors (generic).

procedure axori (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i
int	xor()

begin
	do i = 1, npix {
		c[i] = xor (a[i], b[i])
	}
end

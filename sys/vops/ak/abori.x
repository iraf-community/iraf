# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABOR -- Compute the bitwise boolean 'or' of two vectors (generic).

procedure abori (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i
int	or()

begin
	do i = 1, npix {
		c[i] = or (a[i], b[i])
	}
end

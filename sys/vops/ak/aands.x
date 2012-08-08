# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAND -- Compute the bitwise boolean 'and' of two vectors (generic).

procedure aands (a, b, c, npix)

short	a[ARB], b[ARB], c[ARB]
int	npix, i
short	ands()

begin
	do i = 1, npix {
		c[i] = ands (a[i], b[i])
	}
end

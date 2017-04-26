# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXORK -- Compute the boolean or of a vector and a constant (generic).

procedure axorks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i
short	xors()

begin
	do i = 1, npix {
		c[i] = xors (a[i], b)
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABORK -- Compute the bitwise boolean or of a vector and a constant
# (generic).

procedure aborks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i
short	ors()

begin
	do i = 1, npix {
		c[i] = ors (a[i], b)
	}
end

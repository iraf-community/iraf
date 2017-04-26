# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AANDK -- Compute the bitwise boolean 'and' of a vector and a constant
# (generic)

procedure aandks (a, b, c, npix)

short	a[ARB]
short	b
short	c[ARB]
int	npix, i
short	ands()

begin
	do i = 1, npix {
		c[i] = ands (a[i], b)
	}
end

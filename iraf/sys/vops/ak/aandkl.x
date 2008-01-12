# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AANDK -- Compute the bitwise boolean 'and' of a vector and a constant
# (generic)

procedure aandkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i
long	andl()

begin
	do i = 1, npix {
		c[i] = andl (a[i], b)
	}
end

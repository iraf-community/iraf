# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AANDK -- Compute the bitwise boolean 'and' of a vector and a constant
# (generic)

procedure aandki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i
int	and()

begin
	do i = 1, npix {
		c[i] = and (a[i], b)
	}
end

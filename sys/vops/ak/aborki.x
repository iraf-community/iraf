# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABORK -- Compute the bitwise boolean or of a vector and a constant
# (generic).

procedure aborki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i
int	or()

begin
	do i = 1, npix {
		c[i] = or (a[i], b)
	}
end

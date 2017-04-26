# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABORK -- Compute the bitwise boolean or of a vector and a constant
# (generic).

procedure aborkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i
long	orl()

begin
	do i = 1, npix {
		c[i] = orl (a[i], b)
	}
end

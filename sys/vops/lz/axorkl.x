# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXORK -- Compute the boolean or of a vector and a constant (generic).

procedure axorkl (a, b, c, npix)

long	a[ARB]
long	b
long	c[ARB]
int	npix, i
long	xorl()

begin
	do i = 1, npix {
		c[i] = xorl (a[i], b)
	}
end

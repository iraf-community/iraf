# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AXORK -- Compute the boolean or of a vector and a constant (generic).

procedure axorki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i
int	xor()

begin
	do i = 1, npix {
		c[i] = xor (a[i], b)
	}
end

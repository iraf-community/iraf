# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMULK -- Multiply a constant times a vector (generic).

procedure amulki (a, b, c, npix)

int	a[ARB]
int	b
int	c[ARB]
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] * b
end

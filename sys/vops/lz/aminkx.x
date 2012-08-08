# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMINK -- Compute the minimum of a constant and a vector (generic).

procedure aminkx (a, b, c, npix)

complex	a[ARB]
complex	b
complex	c[ARB]
int	npix, i
real	abs_b

begin
	abs_b = abs (b)

	do i = 1, npix
		if (abs(a[i]) <= abs_b)
		    c[i] = a[i]
		else
		    c[i] = b
end

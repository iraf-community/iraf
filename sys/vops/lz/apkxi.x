# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# APKX -- Generate a type COMPLEX output vector given the real and imaginary
# components as input vectors.

procedure apkxi (a, b, c, npix)

int	a[ARB]			# real component
int	b[ARB]			# imaginary component
complex	c[ARB]			# output vector
int	npix, i

begin
	do i = 1, npix
		c[i] = complex (real(a[i]), real(b[i]))
end

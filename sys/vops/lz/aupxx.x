# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AUPX -- Unpack the real and imaginary components of a complex vector into
# two output vectors of some other type.

procedure aupxx (a, b, c, npix)

complex	a[ARB]			# input vector
complex	b[ARB], c[ARB]	# output vectors
int	npix
int	i

begin
	do i = 1, npix {
		b[i] = complex (real(a[i]), 0.0)
		c[i] = complex (0.0, aimag(a[i]))
	}
end

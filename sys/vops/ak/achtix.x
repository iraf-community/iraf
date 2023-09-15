# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtix (a, b, npix)

int	a[ARB]
complex	b[ARB]
int	npix
int	i

begin
		do i = 1, npix
			b[i] = complex(real(a[i]),0.0)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtdl (a, b, npix)

double	a[ARB]
long	b[ARB]
int	npix
int	i

begin
	call amovd (a, b, npix)
		do i = 1, npix
			b[i] = complex(real(a[i]),0.0)
			b[i] = a[i]
		do i = npix, 1, -1
			b[i] = complex(real(a[i]),0.0)
			b[i] = a[i]
end
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtll (a, b, npix)

long	a[ARB]
long	b[ARB]
int	npix

begin
	call amovl (a, b, npix)
end

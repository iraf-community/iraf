# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtii (a, b, npix)

int	a[ARB]
int	b[ARB]
int	npix

begin
	call amovi (a, b, npix)
end

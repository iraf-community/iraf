# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtdd (a, b, npix)

double	a[ARB]
double	b[ARB]
int	npix

begin
	call amovd (a, b, npix)
end

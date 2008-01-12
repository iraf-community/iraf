# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHTxy -- Change datatype of vector from "x" to "y" (doubly generic).
# The operation is performed in such a way that the output vector can be
# the same as the input vector without overwriting data.

procedure achtss (a, b, npix)

short	a[ARB]
short	b[ARB]
int	npix

begin
	call amovs (a, b, npix)
end

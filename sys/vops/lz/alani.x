# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALAN -- Compute the logical AND of two vectors (generic).  The logical
# output value is returned as an int.

procedure alani (a, b, c, npix)

int	a[ARB], b[ARB]
int	c[ARB]

int	npix, i

begin
	do i = 1, npix
	    if (a[i] != 0 && b[i] != 0)
		c[i] = YES
	    else
		c[i] = NO
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALOV -- Compute the low value (minimum) of a vector.

int procedure alovi (a, npix)

int	a[ARB]
int	npix
int	low, pixval
int	i

begin
	low = a[1]

	do i = 1, npix {
	    pixval = a[i]
	    if (pixval < low)
		low = pixval
	}

	return (low)
end

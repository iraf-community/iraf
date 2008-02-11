# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALOV -- Compute the low value (minimum) of a vector.

short procedure alovs (a, npix)

short	a[ARB]
size_t	npix
short	low, pixval
size_t	i

begin
	low = a[1]

	do i = 1, npix {
	    pixval = a[i]
	    if (pixval < low)
		low = pixval
	}

	return (low)
end

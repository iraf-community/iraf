# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALOV -- Compute the low value (minimum) of a vector.

complex procedure alovx (a, npix)

complex	a[ARB]
int	npix
complex	low, pixval
real	abs_low
int	i

begin
	low = a[1]
	abs_low = abs (low)

	do i = 1, npix {
	    pixval = a[i]
	    if (abs (pixval) < abs_low) {
		low = pixval
		abs_low = abs (low)
	    }
	}

	return (low)
end

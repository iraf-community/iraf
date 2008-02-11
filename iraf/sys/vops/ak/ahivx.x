# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AHIV -- Compute the high value (maximum) of a vector.

complex procedure ahivx (a, npix)

complex	a[ARB]
size_t	npix
complex	high, pixval
real	abs_high
size_t	i

begin
	high = a[1]
	abs_high = abs (high)

	do i = 1, npix {
	    pixval = a[i]
	    if (abs (pixval) > abs_high) {
		high = pixval
		abs_high = abs (high)
	    }
	}

	return (high)
end

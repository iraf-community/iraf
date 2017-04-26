# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AHIV -- Compute the high value (maximum) of a vector.

short procedure ahivs (a, npix)

short	a[ARB]
int	npix
short	high, pixval
int	i

begin
	high = a[1]

	do i = 1, npix {
	    pixval = a[i]
	    if (pixval > high)
		high = pixval
	}

	return (high)
end

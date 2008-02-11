# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AHIV -- Compute the high value (maximum) of a vector.

int procedure ahivi (a, npix)

int	a[ARB]
size_t	npix
int	high, pixval
size_t	i

begin
	high = a[1]

	do i = 1, npix {
	    pixval = a[i]
	    if (pixval > high)
		high = pixval
	}

	return (high)
end

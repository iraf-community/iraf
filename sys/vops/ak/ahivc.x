# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AHIV -- Compute the high value (maximum) of a vector.

char procedure ahivc (a, npix)

char	a[ARB]
int	npix
char	high, pixval
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

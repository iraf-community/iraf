# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALIM -- Compute the limits (minimum and maximum values) of a vector.

procedure aliml (a, npix, minval, maxval)

long	a[ARB], minval, maxval, value
int	npix, i

begin
	minval = a[1]
	maxval = a[1]

	do i = 1, npix {
	    value = a[i]
		if (value < minval)
		    minval = value
		else if (value > maxval)
		    maxval = value
	}
end

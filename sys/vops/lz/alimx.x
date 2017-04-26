# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALIM -- Compute the limits (minimum and maximum values) of a vector.

procedure alimx (a, npix, minval, maxval)

complex	a[ARB], minval, maxval, value
int	npix, i

begin
	minval = a[1]
	maxval = a[1]

	do i = 1, npix {
	    value = a[i]
		if (abs(value) < abs(minval))
		    minval = value
		else if (abs(value) > abs(maxval))
		    maxval = value
	}
end

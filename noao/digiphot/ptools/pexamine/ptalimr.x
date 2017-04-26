include <mach.h>

# PT_ALIMR -- Compute the limits (minimum and maximum values) of a vector
# after rejecting any INDEF valued points.

procedure pt_alimr (a, npix, minval, maxval)

real	a[ARB]			# the input array
int	npix			# the number of points
real	minval, maxval		# the minimum and maximum value

int	i, ngood
real	value

begin
	minval = MAX_REAL
	maxval = -MAX_REAL
	ngood = 0

	do i = 1, npix {
	    value = a[i]
	    if (IS_INDEFR(value))
		next
	    ngood = ngood + 1
	    if (value < minval)
		minval = value
	    if (value > maxval)
		maxval = value
	}

	if (ngood == 0) {
	    minval = INDEFR
	    maxval = INDEFR
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AWVG -- Compute the mean and standard deviation (sigma) of a sample.  Pixels
# whose value lies outside the specified lower and upper limits are not used.
# If the upper and lower limits have the same value (e.g., zero), no limit
# checking is performed.  The number of pixels in the sample is returned as the
# function value.

int procedure awvgr (a, npix, mean, sigma, lcut, hcut)

real	a[ARB]
real	mean, sigma, lcut, hcut
double	sum, sumsq, value, temp
int	npix, i, ngpix

begin
	sum = 0.0
	sumsq = 0.0
	ngpix = 0

	# Accumulate sum, sum of squares.  The test to disable limit checking
	# requires numerical equality of two floating point numbers; this should
	# be ok since they are used as flags not as numbers (they are not used
	# in computations).

	if (hcut == lcut) {
	    do i = 1, npix {
		    value = a[i]
		sum = sum + value
		sumsq = sumsq + value ** 2
	    }
	    ngpix = npix

	} else {
	    do i = 1, npix {
		    value = a[i]
		if (value >= lcut && value <= hcut) {
		    ngpix = ngpix + 1
		    sum = sum + value
		    sumsq = sumsq + value ** 2
		}
	    }
	}

	switch (ngpix) {		# compute mean and sigma
	case 0:
	    mean = INDEFR
	    sigma = INDEFR
	case 1:
	    mean = sum
	    sigma = INDEFR
	default:
	    mean = sum / ngpix
	    temp = (sumsq - (sum/ngpix) * sum) / (ngpix - 1)
	    if (temp < 0)		# possible with roundoff error
		sigma = 0.0
	    else
		sigma = sqrt (temp)
	}

	return (ngpix)
end

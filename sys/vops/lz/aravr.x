# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# ARAV -- Compute the mean and standard deviation of a sample array by
# iteratively rejecting points further than KSIG from the mean.  If the
# value of KSIG is given as 0.0, a cutoff value will be automatically
# calculated from the standard deviation and number of points in the sample.
# The number of pixels remaining in the sample upon termination is returned
# as the function value.

int procedure aravr (a, npix, mean, sigma, ksig)

real	a[ARB]			# input data array
real	mean, sigma, ksig, deviation, lcut, hcut, lgpx
int	npix, ngpix, old_ngpix, awvgr()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean, sigma and reject outliers until no
	# more pixels are rejected, or until there are no more pixels.

	repeat {
	    old_ngpix = ngpix
	    ngpix = awvgr (a, npix, mean, sigma, lcut, hcut)
		if (ngpix <= 1 || sigma <= EPSILONR)
		    break

	    if (ksig == 0.0) {				# Chauvenet's relation
		lgpx = log10 (real(ngpix))
		deviation = (lgpx * (-0.1042 * lgpx + 1.1695) + .8895) * sigma
	    } else
		deviation = sigma * abs(ksig)

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation

	} until (ngpix >= old_ngpix)

	return (ngpix)
end

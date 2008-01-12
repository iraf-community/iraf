define	MAX_ITERATIONS	10
include	<mach.h>

# HD_ARAV -- Compute the mean and standard deviation of a sample array by
# iteratively rejecting points further than KSIG from the mean.  If the
# value of KSIG is given as 0.0, a cutoff value will be automatically
# calculated from the standard deviation and number of points in the sample.
# The number of pixels remaining in the sample upon termination is returned
# as the function value.
#
# A max_iterations parameter was added to prevent the rejection scheme
# from oscillating endlessly for nearly saturated pixels.  This is the
# only difference between the vops procedure and hd_aravr.  (ShJ 5/88)

int procedure hd_aravr (a, npix, mean, sigma, ksig)

real	a[ARB]			# input data array
real	mean, sigma, ksig, deviation, lcut, hcut, lgpx
int	npix, niter, ngpix, old_ngpix, awvgr()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = 0
	niter = 0

	# Iteratively compute mean, sigma and reject outliers until no
	# more pixels are rejected, or until there are no more pixels,
	# or until the maximum iterations limit is exceeded.

	repeat {
	    niter = niter + 1
	    old_ngpix = ngpix
	    ngpix = awvgr (a, npix, mean, sigma, lcut, hcut)
	    if (ngpix <= 1)
		break

	    if (ksig == 0.0) {				# Chauvenet's relation
		lgpx = log10 (real(ngpix))
		deviation = (lgpx * (-0.1042 * lgpx + 1.1695) + .8895) * sigma
	    } else
		deviation = sigma * abs(ksig)

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation

	} until ((old_ngpix == ngpix) || (niter > MAX_ITERATIONS))

	return (ngpix)
end

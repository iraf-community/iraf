include "../lib/fitsky.h"

# AP_MEAN -- Procedure to calculate the mean of the sky pixel array.

int procedure ap_mean (skypix, coords, nskypix, snx, sny, k1, rgrow,
    maxiter, sky_mean, sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]	# unsorted array of skypixels
int	coords[ARB]	# coordinate array
int	nskypix		# total number of sky pixels
int	snx, sny	# dimensions of the sky subraster
real	k1		# number of sky_sigma for rejection
real	rgrow		# radius of region growing
int	maxiter		# maximum number of cycles of rejection
real	sky_mean	# computed sky value
real	sky_sigma	# the computed sky sigma
real	sky_skew	# skewness of sky distribution
int	nsky		# the number of sky pixels used
int	nsky_reject	# the number of sky pixels rejected

double  sumpx, sumsqpx, sumcbpx
int	i
pointer	sp, wgt 
real	locut, hicut
int	ap_grow_regions()

begin
	# Intialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mean = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute the mean, sigma and skew.
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mean,
	    sky_sigma, sky_skew)
	if (maxiter < 1 || k1 <= 0.0 || sky_sigma <= 0.0)
	    return (AP_OK)

	# Allocate space for a weight array.
	call smark (sp)
	call salloc (wgt, nskypix, TY_REAL)
	call amovkr (1.0, Memr[wgt], nskypix)

	# Reject points within k1 * sky_sigma of the median.
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    locut = sky_mean - k1 * sky_sigma
	    hicut = sky_mean + k1 * sky_sigma

	    if (Memr[wgt+i-1] <= 0.0)
		next

	    if (skypix[i] < locut || skypix[i] > hicut) {
		if (rgrow > 0.0) {
		    nsky_reject = nsky_reject + ap_grow_regions (skypix,
		        coords, Memr[wgt], nskypix, i, snx, sny, rgrow,
			sumpx, sumsqpx, sumcbpx)
		} else {
		    sumpx = sumpx - skypix[i]
		    sumsqpx = sumsqpx - skypix[i] ** 2
		    sumcbpx = sumcbpx - skypix[i] ** 3
		    Memr[wgt+i-1] = 0.0 
		    nsky_reject = nsky_reject + 1
		}
	    }

	    nsky = nskypix - nsky_reject
	    if (nsky <= 0)
		break

	    # Recompute the mean, sigma and skew.
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean, sky_sigma,
	        sky_skew)
	    if (sky_sigma <= 0.0)
		break
	}

	# Return an appropriate error code.
	call sfree (sp)
	if (nsky == 0 || nsky_reject == nskypix) {
	    nsky = 0
	    nsky_reject = nskypix
	    sky_mean = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else
	    return (AP_OK)
end

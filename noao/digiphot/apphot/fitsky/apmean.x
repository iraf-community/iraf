include <mach.h>
include "../lib/fitsky.h"

# AP_MEAN -- Procedure to calculate the mean of the sky pixel array.

int procedure ap_mean (skypix, coords, wgt, index, nskypix, snx, sny, losigma,
	hisigma, rgrow, maxiter, sky_mean, sky_sigma, sky_skew, nsky,
	nsky_reject)

real	skypix[ARB]		# unsorted array of skypixels
int	coords[ARB]		# coordinate array for region growing
real	wgt[ARB]		# the weight array for rejection
int	index[ARB]		# sorted array of indices
int	nskypix			# total number of sky pixels
int	snx, sny		# dimensions of the sky subraster
real	losigma, hisigma	# number of sky_sigma for rejection
real	rgrow			# radius of region growing
int	maxiter			# maximum number of cycles of rejection
real	sky_mean		# computed sky value
real	sky_sigma		# the computed sky sigma
real	sky_skew		# skewness of sky distribution
int	nsky			# the number of sky pixels used
int	nsky_reject		# the number of sky pixels rejected

double  dsky, sumpx, sumsqpx, sumcbpx
int	i, j, nreject
real	sky_zero, dmin, dmax, locut, hicut
int	ap_grow_regions()
real	ap_asumr()

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
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	sky_mean = max (dmin, min (sky_mean, dmax))

	# Decide whether to do the rejection cycle.
	if (maxiter < 1 || (IS_INDEFR(losigma) && IS_INDEFR(hisigma)) ||
	    sky_sigma <= 0.0)
	    return (AP_OK)

	# Reject points within k1 * sky_sigma of the median.
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    if (IS_INDEFR(losigma))
		locut = max (-MAX_REAL, dmin)
	    else if (i == 1)
		locut = sky_mean -  min (sky_mean - dmin, dmax - sky_mean,
		    losigma * sky_sigma)
	    else
	        locut = sky_mean - losigma * sky_sigma
	    if (IS_INDEFR(hisigma))
		hicut = min (MAX_REAL, dmax)
	    else if (i == 1)
		hicut = sky_mean + min (dmax - sky_mean, sky_mean - dmin,
		    hisigma * sky_sigma)
	    else
	        hicut = sky_mean + hisigma * sky_sigma

	    nreject = 0
	    do j = 1, nskypix {
	        if (wgt[index[j]] <= 0.0)
		    next
	        if (skypix[index[j]] < locut || skypix[index[j]] > hicut) {
		    if (rgrow > 0.0) {
		        nreject = ap_grow_regions (skypix, coords, wgt,
			    nskypix, sky_zero, index[j], snx, sny, rgrow,
			    sumpx, sumsqpx, sumcbpx)
		    } else {
			dsky = (skypix[index[j]] - sky_zero)
		        sumpx = sumpx - dsky
		        sumsqpx = sumsqpx - dsky ** 2
		        sumcbpx = sumcbpx - dsky ** 3
		        wgt[index[j]] = 0.0 
		        nreject = nreject + 1
		    }
	        }
	    }

	    # Test the number of rejected pixels.
	    if (nreject <= 0)
		break
	    nsky_reject = nsky_reject + nreject

	    # Test that some pixels actually remain.
	    nsky = nskypix - nsky_reject
	    if (nsky <= 0)
		break

	    # Recompute the mean, sigma and skew.
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_sigma, sky_skew)
	    if (sky_sigma <= 0.0)
		break
	    sky_mean = max (dmin, min (sky_mean, dmax))
	}

	# Return an appropriate error code.
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

include "../lib/fitsky.h"

define	MEDCUT	5

# AP_MEDIAN -- Procedure to calculate the median of the sky pixel array.

int procedure ap_median (skypix, coords, index, nskypix, snx, sny, k1, rgrow,
    maxiter, sky_med, sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]	# unsorted array of skypixels
int	coords[ARB]	# coordinate array
int	index[ARB]	# indices in sort order
int	nskypix		# total number of sky pixels
int	snx, sny	# dimensions of the sky subraster
real	k1		# number of sky_sigma for rejection
real	rgrow		# radius of region growing
int	maxiter		# maximum number of cycles of rejection
real	sky_med		# computed sky value
real	sky_sigma	# the computed sky sigma
real	sky_skew	# skewness of sky distribution
int	nsky		# the number of sky pixels used
int	nsky_reject	# the number of sky pixels rejected

double  sumpx, sumsqpx, sumcbpx
int	i, j, ilo, ihi, il, ih, med
pointer	sp, wgt 
real	sky_mean, locut, hicut
int	ap_grow_regions(), apimed()
real	apsmed(), apwsmed()

begin
	# Intialize.
	nsky = nskypix
	nsky_reject = 0
	sky_med = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Sort the sky pixels and compute the median, mean, sigma and skew.
	# MEDCUT tries to correct for quantization effects
	call apqsort (skypix, index, index, nskypix)
	sky_med = apsmed (skypix, index, nskypix, MEDCUT)
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mean,
	    sky_sigma, sky_skew)
	if (maxiter < 1 || k1 <= 0.0 || sky_sigma <= 0.0)
	    return (AP_OK)

	# Allocate space for a weight array.
	call smark (sp)
	call salloc (wgt, nskypix, TY_REAL)
	call amovkr (1.0, Memr[wgt], nskypix)

	# Reject points within k1 * sky_sigma of the median.
	ilo = 1
	ihi = nskypix
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    locut = sky_med - k1 * sky_sigma
	    hicut = sky_med + k1 * sky_sigma

	    # Detect pixels to be rejected.
	    for (il = ilo; il <= nskypix; il = il + 1) {
		if (skypix[index[il]] >= locut)
		    break
	    }
	    for (ih = ihi; ih >= 1; ih = ih - 1) {
		if (skypix[index[ih]] <= hicut)
		    break
	    }
	    if (il == ilo && ih == ihi)
		break

	    # Reject pixels with optional region growing.
	    if (rgrow > 0.0) {

		# Reject low side pixels with region growing.
	        do j = ilo, il - 1 
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        Memr[wgt], nskypix, index[j], snx, sny, rgrow, sumpx,
			sumsqpx, sumcbpx)

		# Reject high side pixels with region growing.
		do j = ih + 1, ihi
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        Memr[wgt], nskypix, index[j], snx, sny, rgrow, sumpx,
			sumsqpx, sumcbpx)

		# Recompute the median.
		# and the median pixel
		nsky = nskypix - nsky_reject
		med = apimed (Memr[wgt], index, il, ih, (nsky + 1) / 2)


	    } else {

		# Recompute the number of sky pixels, the number of rejected
		# pixels and median pixel
		nsky_reject = nsky_reject + (il - ilo) + (ihi - ih)
		nsky = nskypix - nsky_reject
		med = (ih + il) / 2

		# Reject pixels on the low side.
		do j = ilo, il - 1 {
		    sumpx = sumpx - skypix[index[j]]
		    sumsqpx = sumsqpx - skypix[index[j]] ** 2
		    sumcbpx = sumcbpx - skypix[index[j]] ** 3
		}

		# Reject pixels on the high side.
		do j = ih + 1, ihi {
		    sumpx = sumpx - skypix[index[j]]
		    sumsqpx = sumsqpx - skypix[index[j]] ** 2
		    sumcbpx = sumcbpx - skypix[index[j]] ** 3
		}
	    }
	    if (nsky <= 0)
		break

	    # Recompute the median, sigma and skew.
	    sky_med = apwsmed (skypix, index, Memr[wgt], nskypix, med, MEDCUT)
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean, sky_sigma,
	        sky_skew)
	    if (sky_sigma <= 0.0)
		break
	    ilo = il
	    ihi = ih
	}

	# Return an appropriate error code.
	call sfree (sp)
	if (nsky == 0 || nsky_reject == nskypix) {
	    nsky = 0
	    nsky_reject = nskypix
	    sky_med = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else
	    return (AP_OK)
end

include "../lib/fitsky.h"

define	MEDCUT		5

# AP_MODE -- Procedure to calculate the mode of the sky pixel array.

int procedure ap_mode (skypix, coords, index, nskypix, snx, sny, k1, rgrow,
    maxiter, sky_mode, sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]	# unsorted array of skypixels
int	coords[ARB]	# coordinate array
int	index[ARB]	# indices in sort order
int	nskypix		# total number of sky pixels
int	snx, sny	# dimensions of the sky subraster
real	k1		# number of sky_sigma for rejection
real	rgrow		# radius of region growing
int	maxiter		# maximum number of cycles of rejection
real	sky_mode	# computed sky value
real	sky_sigma	# the computed sky sigma
real	sky_skew	# skew of sky pixels
int	nsky		# the number of sky pixels used
int	nsky_reject	# the number of sky pixels rejected

double	sumpx, sumsqpx, sumcbpx
int	i, j, ilo, ihi, il, ih, med
pointer	sp, wgt 
real	locut, hicut, sky_mean, sky_med
int	ap_grow_regions(), apimed()
real	apsmed(), apwsmed()

begin
	# Intialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute the median, sigma, skew and sky mode.
	call apqsort (skypix, index, index, nskypix)
	sky_med = apsmed (skypix, index, nskypix, MEDCUT)
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mean,
	    sky_sigma, sky_skew)
	sky_mode = 3.0 * sky_med - 2.0 * sky_mean
	if (maxiter < 1 || k1 <= 0.0 || sky_sigma <= 0.0)
	    return (AP_OK)

	# Allocate space for a weight array.
	call smark (sp)
	call salloc (wgt, nskypix, TY_REAL)
	call amovkr (1.0, Memr[wgt], nskypix)

	# Reject points within k1 * sky_sigma of the mode.
	ilo = 1
	ihi = nskypix
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    locut = sky_med - k1 * sky_sigma
	    hicut = sky_med + k1 * sky_sigma

	    # Perform lower bound pixel rejection.
	    for (il = ilo; il <= nskypix; il = il + 1) {
		if (skypix[index[il]] >= locut)
		    break
	    }

	    # Perform upper bound pixel rejection.
	    for (ih = ihi; ih >= 1; ih = ih - 1) {
		if (skypix[index[ih]] <= hicut)
		    break
	    }
	    if (il == ilo && ih == ihi)
		break

	    # Compute number of rejected pixels with optional region growing.
	    if (rgrow > 0.0) {

		# Reject lower bound pixels with region growing.
	        do j = ilo, il - 1 
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        Memr[wgt], nskypix, index[j], snx, sny, rgrow, sumpx,
			sumsqpx, sumcbpx)

		# Reject upper bound pixels with region growing.
		do j = ih + 1, ihi
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        Memr[wgt], nskypix, index[j], snx, sny, rgrow, sumpx,
			sumsqpx, sumcbpx)

		# Compute the new median.
		nsky = nskypix - nsky_reject
		med = apimed (Memr[wgt], index, il, ihi, (nsky + 1) / 2)

	    } else {

		# Recompute the number of sky pixels, number of rejected
		# pixels and the median.
		nsky_reject = nsky_reject + (il - ilo) + (ihi - ih)
	        nsky = nskypix - nsky_reject
		med = (ih + il) / 2

		# Reject number of lower bound pixels.
		do j = ilo, il - 1 {
		    sumpx = sumpx - skypix[index[j]]
		    sumsqpx = sumsqpx - skypix[index[j]] ** 2
		    sumcbpx = sumcbpx - skypix[index[j]] ** 3
		}

		# Reject number of upper bound pixels.
		do j = ih + 1, ihi {
		    sumpx = sumpx - skypix[index[j]]
		    sumsqpx = sumsqpx - skypix[index[j]] ** 2
		    sumcbpx = sumcbpx - skypix[index[j]] ** 3
		}
	    }
	    if (nsky <= 0)
		break

	    # Recompute mean, median, mode, sigma and skew.
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean,
		sky_sigma, sky_skew)
	    sky_med = apwsmed (skypix, index, Memr[wgt], nskypix, med,
		MEDCUT)
	    sky_mode = 3.0 * sky_med - 2.0 * sky_mean
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
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else
	    return (AP_OK)
end

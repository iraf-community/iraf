include <mach.h>
include "../lib/fitsky.h"

define	MEDCUT		0.025

# AP_MODE -- Procedure to calculate the mode of the sky pixel array.

int procedure ap_mode (skypix, coords, wgt, index, nskypix, snx, sny, losigma,
	hisigma, rgrow, maxiter, sky_mode, sky_sigma, sky_skew, nsky,
	nsky_reject)

real	skypix[ARB]		# unsorted array of skypixels
int	coords[ARB]		# coordinate array for regions growing
real	wgt[ARB]		# array of weights for rejection
int	index[ARB]		# indices in sort order
int	nskypix			# total number of sky pixels
int	snx, sny		# dimensions of the sky subraster
real	losigma, hisigma	# number of sky_sigma for rejection
real	rgrow			# radius of region growing
int	maxiter			# maximum number of cycles of rejection
real	sky_mode		# computed sky value
real	sky_sigma		# the computed sky sigma
real	sky_skew		# skew of sky pixels
int	nsky			# the number of sky pixels used
int	nsky_reject		# the number of sky pixels rejected

double	dsky, sumpx, sumsqpx, sumcbpx
int	i, j, ilo, ihi, il, ih, med, medcut
real	dmin, dmax, locut, hicut, sky_zero, sky_mean, sky_med
int	ap_grow_regions(), apimed()
real	apsmed(), apwsmed(), ap_asumr()

begin
	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute the median, sigma, skew and sky mode.
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	medcut = nint (MEDCUT * real (nskypix))
	sky_med = apsmed (skypix, index, nskypix, medcut)
	sky_med = max (dmin, min (sky_med, dmax))
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	sky_mean = max (dmin, min (sky_mean, dmax))
	if (sky_mean < sky_med)
	    sky_mode = sky_mean
	else
	    sky_mode = 3.0 * sky_med - 2.0 * sky_mean
	sky_mode = max (dmin, min (sky_mode, dmax))
	if (maxiter < 1 || (IS_INDEFR(losigma) && IS_INDEFR(hisigma)) ||
	    sky_sigma <= 0.0)
	    return (AP_OK)

	# Reject points outside losigma * sky_sigma and hisigma * sky_sigma
	# of the mode.
	ilo = 1
	ihi = nskypix
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    if (i == 1) {
		if (IS_INDEFR(losigma))
		    locut = max (-MAX_REAL, dmin)
		else
	            locut = sky_med - min (sky_med - dmin, dmax - sky_med,
		        losigma * sky_sigma)
		if (IS_INDEFR(hisigma))
		    hicut = min (MAX_REAL, dmax)
		else
	            hicut = sky_med + min (sky_med - dmin, dmax - sky_med,
		        hisigma * sky_sigma)
	    } else {
		if (IS_INDEFR(losigma))
		    locut = max (-MAX_REAL, dmin)
		else
	            locut = sky_mode - losigma * sky_sigma
		if (IS_INDEFR(hisigma))
		    hicut = min (MAX_REAL, dmax)
		else
	            hicut = sky_mode + hisigma * sky_sigma
	    }
	    #call eprintf ("i=%d mean=%g median=%g mode=%g locut=%g hicut=%g\n")
		#call pargi (i)
		#call pargr (sky_mean)
		#call pargr (sky_med)
		#call pargr (sky_mode)
		#call pargr (locut)
		#call pargr (hicut)

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
		        wgt, nskypix, sky_zero, index[j], snx, sny, rgrow,
			sumpx, sumsqpx, sumcbpx)

		# Reject upper bound pixels with region growing.
		do j = ih + 1, ihi
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        wgt, nskypix, sky_zero, index[j], snx, sny, rgrow,
			sumpx, sumsqpx, sumcbpx)

		# Compute the new median.
		nsky = nskypix - nsky_reject
		med = apimed (wgt, index, il, ihi, (nsky + 1) / 2)

	    } else {

		# Recompute the number of sky pixels, number of rejected
		# pixels and the median.
		nsky_reject = nsky_reject + (il - ilo) + (ihi - ih)
	        nsky = nskypix - nsky_reject
		med = (ih + il) / 2

		# Reject number of lower bound pixels.
		do j = ilo, il - 1 {
		    dsky = skypix[index[j]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}

		# Reject number of upper bound pixels.
		do j = ih + 1, ihi {
		    dsky = skypix[index[j]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}
	    }
	    if (nsky <= 0)
		break

	    # Recompute mean, median, mode, sigma and skew.
	    medcut = nint (MEDCUT * real (nsky))
	    sky_med = apwsmed (skypix, index, wgt, nskypix, med, medcut)
	    sky_med = max (dmin, min (sky_med, dmax))
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_sigma, sky_skew)
	    sky_mean = max (dmin, min (sky_mean, dmax))
	    if (sky_mean < sky_med)
		sky_mode = sky_mean
	    else
	        sky_mode = 3.0 * sky_med - 2.0 * sky_mean
	    sky_mode = max (dmin, min (sky_mode, dmax))
	    if (sky_sigma <= 0.0)
		break
	    ilo = il
	    ihi = ih
	}

	# Return an appropriate error code.
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

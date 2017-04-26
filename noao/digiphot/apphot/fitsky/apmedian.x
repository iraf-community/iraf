include <mach.h>
include "../lib/fitsky.h"

define	MEDCUT	0.025

# AP_MEDIAN -- Procedure to calculate the median of the sky pixel array.

int procedure ap_median (skypix, coords, wgt, index, nskypix, snx, sny, losigma,
	hisigma, rgrow, maxiter, sky_med, sky_sigma, sky_skew, nsky,
	nsky_reject)

real	skypix[ARB]		# unsorted array of sky pixels
int	coords[ARB]		# coordinate array for regions growing
real	wgt[ARB]		# array of weights for rejections
int	index[ARB]		# sky pixel indices in sort order
int	nskypix			# total number of sky pixels
int	snx, sny		# dimensions of the sky subraster
real	losigma, hisigma	# upper and lower sigma for rejection
real	rgrow			# radius of region growing
int	maxiter			# maximum number of cycles of rejection
real	sky_med			# computed sky value
real	sky_sigma		# the computed sky sigma
real	sky_skew		# skewness of sky distribution
int	nsky			# the number of sky pixels used
int	nsky_reject		# the number of sky pixels rejected

double  dsky, sumpx, sumsqpx, sumcbpx
int	i, j, ilo, ihi, il, ih, med, medcut
real	sky_zero, sky_mean, locut, hicut, dmin, dmax
int	ap_grow_regions(), apimed()
real	apsmed(), apwsmed(), ap_asumr()

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
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	medcut = nint (MEDCUT * real (nskypix))
	sky_med = apsmed (skypix, index, nskypix, medcut)
	sky_med = max (dmin, min (sky_med, dmax))
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	sky_mean = max (dmin, min (sky_mean, dmax))
	if (maxiter < 1 || (IS_INDEFR(losigma) && IS_INDEFR(hisigma)) ||
	    sky_sigma <= 0.0)
	    return (AP_OK)
	#call printf (
	    #"mean=%g med=%g sigma=%g nsky=%d nrej=%d dmin=%g dmax=%g\n")
	    #call pargr (sky_mean)
	    #call pargr (sky_med)
	    #call pargr (sky_sigma)
	    #call pargi (nsky)
	    #call pargi (nsky_reject)
	    #call pargr (dmin)
	    #call pargr (dmax)

	# Reject points outside losigma * sky_sigma and hisigma * sky_sigma
	# of the median.
	ilo = 1
	ihi = nskypix
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    if (IS_INDEFR(losigma))
		locut = max (-MAX_REAL, dmin)
	    else if (i == 1)
		locut = sky_med - min (sky_med - dmin, dmax - sky_med,
		    losigma * sky_sigma)
	    else
	        locut = sky_med - losigma * sky_sigma
	    if (IS_INDEFR(hisigma))
		hicut = min (MAX_REAL, dmax)
	    else if (i == 1)
		hicut = sky_med + min (dmax - sky_med, sky_med - dmin,
		    hisigma * sky_sigma)
	    else
	        hicut = sky_med + hisigma * sky_sigma
	    #call printf ("    locut=%g hicut=%g\n")
		#call pargr (locut)
		#call pargr (hicut)

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
		        wgt, nskypix, sky_zero, index[j], snx, sny, rgrow,
			sumpx, sumsqpx, sumcbpx)

		# Reject high side pixels with region growing.
		do j = ih + 1, ihi
		    nsky_reject = nsky_reject + ap_grow_regions (skypix, coords,
		        wgt, nskypix, sky_zero, index[j], snx, sny, rgrow,
			sumpx, sumsqpx, sumcbpx)

		# Recompute the median.
		# and the median pixel
		nsky = nskypix - nsky_reject
		med = apimed (wgt, index, il, ih, (nsky + 1) / 2)


	    } else {

		# Recompute the number of sky pixels, the number of rejected
		# pixels and median pixel
		nsky_reject = nsky_reject + (il - ilo) + (ihi - ih)
		nsky = nskypix - nsky_reject
		med = (ih + il) / 2

		# Reject pixels on the low side.
		do j = ilo, il - 1 {
		    dsky = skypix[index[j]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}

		# Reject pixels on the high side.
		do j = ih + 1, ihi {
		    dsky = skypix[index[j]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}
	    }
	    if (nsky <= 0)
		break

	    # Recompute the median, sigma and skew.
	    medcut = nint (MEDCUT * real (nsky))
	    sky_med = apwsmed (skypix, index, wgt, nskypix, med, medcut)
	    sky_med = max (dmin, min (sky_med, dmax))
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_sigma, sky_skew)
	    sky_mean = max (dmin, min (sky_mean, dmax))
	    #call printf (
	    #"    mean=%g med=%g sigma=%g nsky=%d nrej=%d dmin=%g dmax=%g\n")
	        #call pargr (sky_mean)
	        #call pargr (sky_med)
	        #call pargr (sky_sigma)
	        #call pargi (nsky)
	        #call pargi (nsky_reject)
	        #call pargr (dmin)
	        #call pargr (dmax)

	    if (sky_sigma <= 0.0)
		break
	    ilo = il
	    ihi = ih
	}

	# Return an appropriate error code.
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

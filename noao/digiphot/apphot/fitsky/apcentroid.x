include <mach.h>
include "../lib/fitsky.h"

# AP_CENTROID -- Procedure to find the mode, width and skew of the sky
# distribution by computing the moments of the histogram. 

int procedure ap_centroid (skypix, coords, wgt, index, nskypix, snx, sny, k1,
	hwidth, binsize, smooth, losigma, hisigma, rgrow, maxiter, sky_mode,
	sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]		# array of sky pixels
int	coords[ARB]		# array of coordinates for regions growing
real	wgt[ARB]		# array of weights for rejection
int	index[ARB]		# array of sorted indices
int	nskypix			# the number of sky pixels
int	snx, sny		# the maximum dimensions of sky raster
real	k1			# extent of the histogram in skysigma
real	hwidth			# width of the histogram
real	binsize			# the size of the histogram in sky sigma
int	smooth			# smooth the histogram before fitting
real	losigma, hisigma	# upper and lower k-sigma rejection limits
real	rgrow			# region growing radius in pixels
int	maxiter			# maximum number of rejection cycles
real	sky_mode		# computed sky value
real	sky_sigma		# computed sigma of the sky pixels
real	sky_skew		# skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of sky pixels rejected

double	dsky, sumpx, sumsqpx, sumcbpx
int	nreject, nbins, nker, ier, i, j
pointer	sp, hgm, shgm
real	dmin, dmax, sky_mean, hmin, hmax, dh, locut, hicut, cut
real	sky_zero
int	ap_grow_hist2(), aphigmr()
real	ap_asumr(), apmedr()

begin
	# Intialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute the histogram width and binsize.
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	sky_mean = apmedr (skypix, index, nskypix)
	sky_mean = max (dmin, min (sky_mean, dmax))
	if (! IS_INDEFR(hwidth) && hwidth > 0.0) {
	    hmin = sky_mean - k1 * hwidth
	    hmax = sky_mean + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    cut = min (sky_mean - dmin, dmax - sky_mean, k1 * sky_sigma)
	    hmin = sky_mean - cut
	    hmax = sky_mean + cut
	    dh = binsize * cut / k1
	}

	# Compute the number of histogram bins and the histogram resolution.
	if (dh <= 0.0) {
	    nbins = 1
	    dh = 0.0
	} else {
	    nbins = 2 * nint ((hmax - sky_mean) / dh) + 1
	    dh = (hmax - hmin) / (nbins - 1)
	}

	# Check for a valid histogram.
	if (nbins < 2 || k1 <= 0.0 || sky_sigma < dh || dh <= 0.0 ||
	    sky_sigma <= 0.0) {
	    sky_mode = sky_mean
	    sky_sigma = 0.0
	    sky_skew = 0.0
	    return (AP_NOHISTOGRAM)
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (hgm, nbins, TY_REAL)
	call salloc (shgm, nbins, TY_REAL)

	# Accumulate the histogram.
	call aclrr (Memr[hgm], nbins)
	nsky_reject = nsky_reject + aphigmr (skypix, wgt, index, nskypix,
	    Memr[hgm], nbins, hmin, hmax)
	nsky = nskypix - nsky_reject

	# Do the initial rejection.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
		if (wgt[index[i]] <= 0.0) {
		    dsky = skypix[index[i]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}
	    }
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero, sky_mean,
	        sky_sigma, sky_skew)
	}

	# Fit the mode, sigma an skew of the histogram.
	if (smooth == YES) {
	    nker = max (1, nint (sky_sigma / dh))
	    #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_imode (Memr[shgm], nbins, hmin, hmax, YES, sky_mode, ier)
	} else
	    call ap_imode (Memr[hgm], nbins, hmin, hmax, NO, sky_mode, ier)
	sky_mode = max (dmin, min (sky_mode, dmax))
	if ((IS_INDEFR(losigma) && IS_INDEFR(hisigma)) || (sky_sigma < dh) ||
	    maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute new histogram limits.
	    if (IS_INDEFR(losigma))
		locut = -MAX_REAL
	    else
	        locut = sky_mode - losigma * sky_sigma
	    if (IS_INDEFR(hisigma))
		hicut = MAX_REAL
	    else
	        hicut = sky_mode + hisigma * sky_sigma

	    # Reject pixels.
	    nreject = 0
	    do j = 1, nskypix {
		if (skypix[index[j]] >= locut && skypix[index[j]] <= hicut)
		    next
		if (rgrow > 0.0)
		    nreject = nreject + ap_grow_hist2 (skypix, coords,
			wgt, nskypix, sky_zero, index[j], snx, sny, Memr[hgm],
			nbins, hmin, hmax, rgrow, sumpx, sumsqpx, sumcbpx)
		else if (wgt[index[j]] > 0.0) {
		    call ap_hgmsub2 (Memr[hgm], nbins, hmin, hmax,
		        skypix[index[j]], sky_zero, sumpx, sumsqpx, sumcbpx)
		    wgt[index[j]] = 0.0
		    nreject = nreject + 1
		}
	    }
	    if (nreject == 0)
		break
	    nsky_reject = nsky_reject + nreject
	    nsky = nskypix - nsky_reject 
	    if (nsky <= 0)
		break

	    # Recompute the moments.
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero, sky_mean,
	        sky_sigma, sky_skew)

	    # Recompute the histogram peak.
	    if (smooth == YES) {
		nker = max (1, nint (sky_sigma / dh))
	        #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	        call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	        call ap_imode (Memr[shgm], nbins, hmin, hmax, YES, sky_mode,
		    ier)
	    } else
	        call ap_imode (Memr[hgm], nbins, hmin, hmax, NO, sky_mode, ier)
	    sky_mode = max (dmin, min (sky_mode, dmax))
	    if (sky_sigma <= dh || ier != AP_OK)
		break
	}

	# Return the error codes.
	call sfree (sp)
	if (nsky == 0 || nsky_reject == nskypix || ier == AP_NOHISTOGRAM) {
	    nsky = 0
	    nsky_reject = nskypix
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else
	    return (AP_OK)
end


# AP_IMODE -- Procedure to compute the 1st, 2nd and third moments of the
# histogram of sky pixels.

procedure ap_imode (hgm, nbins, z1, z2, smooth, sky_mode, ier)

real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# min and max of the histogram
int	smooth			# is the histogram smoothed
real	sky_mode		# mode of histogram
int	ier			# error code

int	i, noccup
double	sumi, sumix, x, dh
real	hmean, dz
real	asumr()

begin
	# Initialize the sums.
	sumi = 0.0
	sumix = 0.0

	# Compute a continuum level.
	if (smooth == NO) 
	    hmean = asumr (hgm, nbins) / nbins
	else {
	    call alimr (hgm, nbins, dz, hmean)
	    hmean = 2.0 * hmean / 3.0
	}

	# Accumulate the sums.
	noccup = 1
	dz = (z2 - z1) / (nbins - 1)
	x = z1 + 0.5 * dz
	do i = 1, nbins {
	    dh = hgm[i] - hmean
	    if (dh > 0.0d0) {
	        sumi = sumi + dh
	        sumix = sumix + dh * x
	        noccup = noccup + 1
	    }
	    x = x + dz
	}

	# Compute the sky mode, sigma and skew.
	if (sumi > 0.0) {
	    sky_mode = sumix / sumi
	    ier = AP_OK
	} else {
	    sky_mode = INDEFR
	    ier = AP_NOHISTOGRAM
	}
end

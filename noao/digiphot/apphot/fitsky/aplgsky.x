include <mach.h>
include "../lib/fitsky.h"

define	TOL		0.001		# Fitting tolerance

# AP_LGSKY -- Procedure to fit the peak and width of the histogram using
# repeated convolutions and a triangle function.

int procedure ap_lgsky (skypix, coords, wgt, index, nskypix, snx, sny, k1,
	hwidth, binsize, smooth, losigma, hisigma, rgrow, maxiter, sky_mode,
	sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]		# array of sky pixels
int	coords[ARB]		# array of coordinates of region growing
real	wgt[ARB]		# array of weights for rejection
int	index[ARB]		# array of sort indices
int	nskypix			# the number of sky pixels
int	snx, sny		# the maximum dimensions of sky raster
real	k1			# extent of the histogram in skysigma
real	hwidth			# width of histogram
real	binsize			# the size of the histogram in sky sigma
int	smooth			# smooth the histogram before fitting
real	losigma, hisigma	# upper and lower sigma rejection limits
real	rgrow			# region growing radius in pixels
int	maxiter			# maximum number of rejection cycles
real	sky_mode		# computed sky value
real	sky_sigma		# computed sigma of the sky pixels
real	sky_skew		# skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of sky pixels rejected

double 	dsky, sumpx, sumsqpx, sumcbpx
int	nreject, nbins, nker, i, j, iter
pointer	sp, hgm, shgm
real	dmin, dmax, hmin, hmax, dh, locut, hicut, sky_mean, center, cut
real	sky_zero
int	ap_grow_hist2(), aphigmr(), aptopt()
real	ap_asumr(), apmedr(), apmapr()

begin
	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute a first guess for the parameters.
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	sky_mean = apmedr (skypix, index, nskypix)
	sky_mean = max (dmin, min (sky_mean, dmax))

	# Compute the width and bin size of histogram.
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

	# Compute the number of histogram bins and the resolution.
	# filter.
	if (dh <= 0.0) {
	    nbins = 1
	    dh = 0.0
	} else {
	    nbins = 2 * nint ((hmax - sky_mean) / dh) + 1
	    dh  = (hmax - hmin) / (nbins - 1)
	}

	# Test for a valid histogram.
	if (nbins < 2 || k1 <= 0.0 || sky_sigma <= 0.0 || dh <= 0.0 ||
	    sky_sigma <= dh) {
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

	# Perform the initial rejection.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
	        if (wgt[index[i]] <= 0.0) {
		    dsky = skypix[index[i]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		}
	    }
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_sigma, sky_skew)
	}

	# Fit the peak of the histogram.
	center = apmapr ((hmin + hmax) / 2.0, hmin + 0.5 * dh,
	    hmax + 0.5 * dh, 1.0, real (nbins))
	if (smooth == YES) {
	    nker = max (1, nint (sky_sigma / dh))
	    #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    iter =  aptopt (Memr[shgm], nbins, center, sky_sigma / dh,
	        TOL, maxiter, NO)
	} else
	    iter = aptopt (Memr[hgm], nbins, center, sky_sigma / dh, TOL,
	        maxiter, NO)
	sky_mode = apmapr (center, 1.0, real (nbins), hmin + 0.5 * dh,
	    hmax + 0.5 * dh)
	sky_mode = max (dmin, min (sky_mode, dmax))
	if (iter < 0) {
	    call sfree (sp)
	    return (AP_SKY_NOCONVERGE)
	}
	if ((IS_INDEFR(losigma) && IS_INDEFR(hisigma)) || (sky_sigma <= dh) ||
	    (maxiter < 1)) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit the histogram with pixel rejection and optional region growing.
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

	    # Detect and reject the pixels.
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

	    # Recompute the data limits.
	    nsky_reject = nsky_reject + nreject
	    nsky = nskypix - nsky_reject 
	    if (nsky <= 0)
		break
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_sigma, sky_skew)
	    if (sky_sigma <= dh)
		break

	    # Refit the sky.
	    if (smooth == YES) {
		nker = max (1, nint (sky_sigma / dh))
	        #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	        call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    	iter = aptopt (Memr[shgm], nbins, center, sky_sigma / dh,
		    TOL, maxiter, NO)
	    } else
	    	iter = aptopt (Memr[hgm], nbins, center, sky_sigma / dh,
		    TOL, maxiter, NO)
	    sky_mode = apmapr (center, 1.0, real (nbins), hmin + 0.5 * dh,
	        hmax + 0.5 * dh)
	    sky_mode = max (dmin, min (sky_mode, dmax))
	    if (iter < 0) 
		break
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
	} else if (sky_sigma <= 0.0) {
	    sky_sigma = 0.0
	    sky_skew = 0.0
	    return (AP_OK)
	} else if (iter < 0) {
	    return (AP_SKY_NOCONVERGE)
	} else {
	    return (AP_OK)
	}
end

include "../lib/fitsky.h"

define	SFRACTION	.20		# Lucy smoothing length
define	TOL		0.001		# Fitting tolerance

# AP_LGSKY -- Procedure to fit the peak and width of the histogram using
# repeated convolutions and a triangle function.

int procedure ap_lgsky (skypix, coords, nskypix, snx, sny,
    k1, hwidth, binsize, smooth, k2, rgrow, maxiter, sky_mode, sky_sigma,
    sky_skew, nsky, nsky_reject)

real	skypix[ARB]		# array of sky pixels
int	coords[ARB]		# array of coordinates
int	nskypix			# the number of sky pixels
int	snx, sny		# the maximum dimensions of sky raster
real	k1			# extent of the histogram in skysigma
real	hwidth			# width of histogram
real	binsize			# the size of the histogram in sky sigma
int	smooth			# smooth the histogram before fitting
real	k2			# rejection criterion for histogram
real	rgrow			# region growing radius in pixels
int	maxiter			# maximum number of rejection cycles
real	sky_mode		# computed sky value
real	sky_sigma		# computed sigma of the sky pixels
real	sky_skew		# skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of sky pixels rejected

double 	sumpx, sumsqpx, sumcbpx
int	nreject, nbins, nker, i, j, iter, ier
pointer	sp, hgm, shgm, wgt
real	hmin, hmax, dh, locut, hicut, sky_mean, center
int	ap_grow_hist2(), aphgmr(), aptopt()
real	apmapr()

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
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mean,
	    sky_sigma, sky_skew)

	# Compute the width and bin size of histogram.
	if (! IS_INDEFR(hwidth)) {
	    hmin = sky_mean - k1 * hwidth
	    hmax = sky_mean + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    hmin = sky_mean - k1 * sky_sigma
	    hmax = sky_mean + k1 * sky_sigma
	    dh = binsize * sky_sigma
	}

	# Compute the number of histogram bins and width of the smoothing
	# filter.
	if (dh <= 0.0) {
	    nbins = 1
	    nker = 1
	} else {
	    nbins = 2 * int ((hmax - sky_mean) / dh) + 1
	    nker = 2 * int (SFRACTION * (hmax - sky_mean) / dh) + 1
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
	call salloc (wgt, nskypix, TY_REAL)

	# Accumulate the histogram.
	call aclrr (Memr[hgm], nbins)
	call amovkr (1.0, Memr[wgt], nskypix)
	nsky_reject = nsky_reject + aphgmr (skypix, Memr[wgt], nskypix,
	    Memr[hgm], nbins, hmin, hmax)
	nsky = nskypix - nsky_reject

	# Perform the initial rejection.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
	        if (Memr[wgt+i-1] <= 0.0) {
		    sumpx = sumpx - skypix[i]
		    sumsqpx = sumsqpx - skypix[i] ** 2
		    sumcbpx = sumcbpx - skypix[i] ** 3
		}
	    }
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean, sky_sigma,
	        sky_skew)
	}

	# Fit the peak of the histogram.
	center = apmapr (sky_mean, hmin, hmax, 1.0, real (nbins))
	if (smooth == YES) {
	    call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    iter =  aptopt (Memr[shgm], nbins, center, sky_sigma / dh,
	        TOL, maxiter, NO)
	} else
	    iter = aptopt (Memr[hgm], nbins, center, sky_sigma / dh, TOL,
	        maxiter, NO)
	sky_mode = apmapr (center, 1.0, real (nbins), hmin, hmax)
	if (iter < 0) {
	    call sfree (sp)
	    return (AP_SKY_NOCONVERGE)
	}
	if (k2 <= 0.0 || sky_sigma <= dh || maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit the histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute new histogram limits.
	    locut = sky_mode - k2 * sky_sigma
	    hicut = sky_mode + k2 * sky_sigma

	    # Detect and reject the pixels.
	    nreject = 0
	    do j = 1, nskypix {
		if (skypix[j] >= locut && skypix[j] <= hicut)
		    next
		if (rgrow > 0.0)
		    nreject = nreject + ap_grow_hist2 (skypix, coords,
			Memr[wgt], nskypix, j, snx, sny, Memr[hgm], nbins,
			hmin, hmax, rgrow, sumpx, sumsqpx, sumcbpx)
		else if (Memr[wgt+j-1] > 0.0) {
		    call ap_hgmsub2 (Memr[hgm], nbins, hmin, hmax, skypix[j],
		        sumpx, sumsqpx, sumcbpx)
		    Memr[wgt+j-1] = 0.0
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
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean, sky_sigma,
	        sky_skew)
	    if (sky_sigma <= dh)
		break

	    # Refit the sky.
	    if (smooth == YES) {
	        call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    	iter = aptopt (Memr[shgm], nbins, center, sky_sigma / dh,
		    TOL, maxiter, NO)
	    } else
	    	iter = aptopt (Memr[hgm], nbins, center, sky_sigma / dh,
		    TOL, maxiter, NO)
	    sky_mode = apmapr (center, 1.0, real (nbins), hmin, hmax)
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
	} else if (ier != AP_OK) {
	    return (ier)
	} else {
	    return (AP_OK)
	}
end

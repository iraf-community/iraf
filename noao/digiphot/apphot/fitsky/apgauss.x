include <mach.h>
include "../lib/nlfit.h"
include "../lib/fitsky.h"

define	SFRACTION	.20		# Lucy smoothing length
define	TOL		.001		# Fitting tolerance

# AP_GAUSS -- Procedure to compute the peak, width and skew of the histogram
# by fitting a skewed Gaussian function to the histogram.

int procedure ap_gauss (skypix, coords, nskypix, snx, sny, maxfit
    k1, hwidth, binsize, smooth, k2, rgrow, maxiter, sky_mode, sky_sigma,
    sky_skew, nsky, nsky_reject)

real	skypix[ARB]	# array of sky pixels
int	coords[ARB]	# array of coordinates
int	nskypix		# the number of sky pixels
int	snx, sny	# the maximum dimensions of sky raster
int	maxfit		# maximum no. iteration per fit
real	k1		# extent of the histogram in skysigma
real	hwidth		# width of the histogram
real	binsize		# the size of the histogram in sky sigma
int	smooth		# smooth the histogram before fitting
real	k2		# rejection criterion for histogram
real	rgrow		# region growing radius in pixels
int	maxiter		# maximum number of rejection cycles
real	sky_mode	# computed sky value
real	sky_sigma	# computed sigma of the sky pixels
real	sky_skew	# skew of sky pixels
int	nsky		# number of sky pixels used in fit
int	nsky_reject	# number of sky pixels rejected

double	sumpx, sumsqpx, sumcbpx
int	i, j, nreject, nbins, nker, ier
pointer	sp, x, hgm, shgm, w, wgt
real	hmin, hmax, dh, locut, hicut
int	ap_grow_hist(), aphgmr()

begin
	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Compute initial guess for sky statistics.
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mode,
	    sky_sigma, sky_skew)

	# Compute the width and bin size of the sky histogram.
	if (! IS_INDEFR(hwidth)) {
	    hmin = sky_mode - k1 * hwidth
	    hmax = sky_mode + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    hmin = sky_mode - k1 * sky_sigma
	    hmax = sky_mode + k1 * sky_sigma
	    dh = binsize * sky_sigma
	}

	# Compute the number of histogram bins and width of smoothing kernel.
	if (dh <= 0.0) {
	    nbins = 1
	    nker = 1
	} else {
	    nbins = 2 * int ((hmax - sky_mode) / dh) + 1
	    nker = 2 * int (SFRACTION * (hmax - sky_mode) / dh) + 1
	}

	# Test for a valid histogram.
	if (sky_sigma <= dh || dh <= 0.0 ||  k1 <= 0.0 || sky_sigma <= 0.0 ||
	    nbins < 4) {
	    sky_sigma = 0.0
	    sky_skew = 0.0
	    return (AP_NOHISTOGRAM)
	}

	# Allocate temporary working space.
	call smark (sp)
	call salloc (x, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)
	call salloc (shgm, nbins, TY_REAL)
	call salloc (w, nbins, TY_REAL)
	call salloc (wgt, nskypix, TY_REAL)

	# Compute the x array.
	do i = 1, nbins
	    Memr[x+i-1] = i
	call amapr (Memr[x], Memr[x], nbins, 1.0, real (nbins), hmin, hmax)

	# Accumulate the histogram.
	call aclrr (Memr[hgm], nbins)
	call amovkr (1.0, Memr[wgt], nskypix)
	nsky_reject = nsky_reject + aphgmr (skypix, Memr[wgt], nskypix,
	    Memr[hgm], nbins, hmin, hmax)
	nsky = nskypix - nsky_reject

	# Do the initial rejection.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
		if (Memr[wgt+i-1] <= 0.0) {
		    sumpx = sumpx - skypix[i]
		    sumsqpx = sumsqpx - skypix[i] ** 2
		    sumcbpx = sumcbpx - skypix[i] ** 3
		}
	    }
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mode,
	        sky_sigma, sky_skew)
	}

	# Find the mode, sigma and skew of the histogram.
	if (smooth == YES) {
	    call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_hist_mode (Memr[x], Memr[shgm], Memr[w], nbins,
		sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	} else
	    call ap_hist_mode (Memr[x], Memr[hgm], Memr[w], nbins,
		sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	if (ier != AP_OK) {
	    call sfree (sp)
	    return (ier)
	}
	if (k2 <= 0.0 || sky_sigma <= dh || maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    locut = sky_mode - k2 * sky_sigma
	    hicut = sky_mode + k2 * sky_sigma

	    # Detect and reject pixels.
	    nreject = 0
	    do j = 1, nskypix {
		if (skypix[j] >= locut && skypix[j] <= hicut)
		    next
		if (rgrow > 0.0)
		    nreject = nreject + ap_grow_hist (skypix, coords,
			Memr[wgt], nskypix, j, snx, sny, Memr[hgm], nbins,
			hmin, hmax, rgrow)
		else if (Memr[wgt+j-1] > 0.0) {
		    call ap_hgmsub (Memr[hgm], nbins, hmin, hmax, skypix[j])
		    Memr[wgt+j-1] = 0.0
		    nreject = nreject + 1
		}
	    }
	    if (nreject == 0)
		break
	    nsky_reject = nsky_reject + nreject
	    nsky = nskypix - nsky_reject 
	    if (nsky <= 0)
		break

	    # Recompute mean, mode, sigma and skew.
	    if (smooth == YES) {
	        call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    	call ap_hist_mode (Memr[x], Memr[shgm], Memr[w], nbins,
		    sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	    } else
	    	call ap_hist_mode (Memr[x], Memr[hgm], Memr[w], nbins,
		    sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	    if (ier != AP_OK)
		break
	    if (sky_sigma <= dh)
		break
	}

	# Return appropriate error code.
	call sfree (sp)
	if (nsky == 0 || nsky_reject == nskypix) {
	    nsky = 0
	    nsky_reject = nskypix
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else if (ier == AP_NSKY_TOO_SMALL) {
	    return (AP_NSKY_TOO_SMALL)
	} else if (ier != AP_OK) {
	    return (ier)
	} else
	    return (AP_OK)
end


# AP_HIST_MODE -- Procedure to fit the skewed Gaussian function to the histogram
# of the sky pixels.

procedure ap_hist_mode (x, hgm, w, nbins, sky_mode, sky_sigma, sky_skew,
    maxiter, tol, ier)

real	x[ARB]		# x array
real	hgm[ARB]	# histogram
real	w[ARB]		# weights
int	nbins		# number of bins
real	sky_mode	# sky value
real	sky_sigma	# sky sigma
real	sky_skew	# sky skew
int	maxiter		# max number of iterations
real	tol		# tolerances
int	ier		# error code

extern  gausskew, dgausskew
int	i, imin, imax, np, fier
pointer	sp, list, nl
real	p[4], dp[4], junk

begin
	# Initialize.
	call smark (sp)
	call salloc (list, 4, TY_INT)
	do i = 1, 4
	    Memi[list+i-1] = i

	# Compute initial guesses for the parameters
	call ap_alimr (hgm, nbins, junk, p[1], imin, imax)
	p[2] = x[imax]
	p[3] = max (sky_sigma ** 2, abs (x[2] - x[1]) ** 2)
	p[4] = 0.0
	np = 4

	# Fit the histogram.
	call nlinit (nl, gausskew, dgausskew, p, dp, np, Memi[list], 4, tol,
	    maxiter)
	call nlfit (nl, x, x, hgm, w, nbins, nbins, 1, WTS_UNIFORM, fier)
	call nlpget (nl, p, np)
	call nlerrors (nl, WTS_UNIFORM, junk, dp)
	call nlfree (nl)
	call sfree (sp)

	# Return the appropriate error code.
	ier = AP_OK
	if (fier == NO_DEG_FREEDOM) {
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    ier = AP_NSKY_TOO_SMALL
	} else {
	    if (fier == SINGULAR)
		ier = AP_SKY_SINGULAR
	    else if (fier == NOT_DONE)
		ier = AP_SKY_NOCONVERGE
	    if (p[2] < x[1] || p[2] > x[nbins]) {
		sky_mode = x[imax]
		ier = AP_BADPARAMS
	    } else
	        sky_mode = p[2]
	    if (p[3] <= 0.0) {
		sky_sigma = 0.0
		sky_skew = 0.0
		ier = AP_BADPARAMS
	    } else {
	        sky_sigma = sqrt (p[3])
	        sky_skew = 1.743875281 * abs (p[4]) ** (1.0 / 3.0) * sky_sigma
	        if (p[4] < 0.0)
		    sky_skew = - sky_skew
	    }
	}
end

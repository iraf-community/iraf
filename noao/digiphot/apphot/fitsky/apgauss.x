include <mach.h>
include <math/nlfit.h>
include "../lib/fitsky.h"

define	TOL		.001		# Fitting tolerance

# AP_GAUSS -- Procedure to compute the peak, width and skew of the histogram
# by fitting a skewed Gaussian function to the histogram.

int procedure ap_gauss (skypix, coords, wgt, index, nskypix, snx, sny, maxfit,
	k1, hwidth, binsize, smooth, losigma, hisigma, rgrow, maxiter,
	sky_mode, sky_sigma, sky_skew, nsky, nsky_reject)

real	skypix[ARB]		# array of unsorted sky pixels
int	coords[ARB]		# array of coordinates for region growing
real	wgt[ARB]		# weights for pixel rejection
int	index[ARB]		# array of sort indices
int	nskypix			# the number of sky pixels
int	snx, sny		# the maximum dimensions of sky raster
int	maxfit			# maximum number of iterations per fit
real	k1			# extent of the histogram in skysigma
real	hwidth			# width of the histogram
real	binsize			# the size of the histogram in sky sigma
int	smooth			# smooth the histogram before fitting
real	losigma, hisigma	# upper and lower sigma rejection criterion
real	rgrow			# region growing radius in pixels
int	maxiter			# maximum number of rejection cycles
real	sky_mode		# computed sky value
real	sky_sigma		# computed sigma of the sky pixels
real	sky_skew		# skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of sky pixels rejected

double	dsky, sumpx, sumsqpx, sumcbpx
int	i, j, nreject, nbins, nker, ier
pointer	sp, x, hgm, shgm, w
real	sky_mean, sky_msigma, dmin, dmax, hmin, hmax, dh, locut, hicut, cut
real	sky_zero
int	ap_grow_hist2(), aphigmr()
real	ap_asumr(), apmedr()

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
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_msigma, sky_skew)
	sky_mean = apmedr (skypix, index,  nskypix)
	sky_mean = max (dmin, min (sky_mean, dmax))

	# Compute the width and bin size of the sky histogram.
	if (! IS_INDEFR(hwidth) && hwidth > 0.0) {
	    hmin = sky_mean - k1 * hwidth
	    hmax = sky_mean + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    cut = min (sky_mean - dmin, dmax - sky_mean, k1 * sky_msigma)
	    hmin = sky_mean - cut
	    hmax = sky_mean + cut
	    dh = binsize * cut / k1
	}

	# Compute the number of histogram bins and width of smoothing kernel.
	if (dh <= 0.0) {
	    nbins = 1
	    dh = 0.0
	} else {
	    nbins = 2 * nint ((hmax - sky_mean) / dh) + 1
	    dh = (hmax - hmin) / (nbins - 1)
	}

	# Test for a valid histogram.
	if (sky_msigma <= dh || dh <= 0.0 ||  k1 <= 0.0 || sky_msigma <= 0.0 ||
	    nbins < 4) {
	    sky_mode = sky_mean
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

	# Compute the x array.
	do i = 1, nbins
	    Memr[x+i-1] = i
	call amapr (Memr[x], Memr[x], nbins, 1.0, real (nbins),
	    hmin + 0.5 * dh, hmax + 0.5 * dh)

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
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_msigma, sky_skew)
	}

	# Find the mode, sigma and skew of the histogram.
	if (smooth == YES) {
	    nker = max (1, nint (sky_msigma / dh))
	    #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_hist_mode (Memr[x], Memr[shgm], Memr[w], nbins,
		sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	} else
	    call ap_hist_mode (Memr[x], Memr[hgm], Memr[w], nbins,
		sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	sky_mode = max (dmin, min (sky_mode, dmax))
	if (ier != AP_OK) {
	    call sfree (sp)
	    return (ier)
	}
	if ((IS_INDEFR(losigma) && IS_INDEFR(hisigma)) || sky_sigma <= dh ||
	    maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute the new rejection limits.
	    if (IS_INDEFR(losigma))
		locut = -MAX_REAL
	    else
	        locut = sky_mode - losigma * sky_msigma
	    if (IS_INDEFR(hisigma))
		hicut = MAX_REAL
	    else
	        hicut = sky_mode + hisigma * sky_msigma

	    # Detect and reject pixels.
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
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero,
	        sky_mean, sky_msigma, sky_skew)

	    # Recompute mean, mode, sigma and skew.
	    if (smooth == YES) {
		nker = max (1, nint (sky_msigma / dh))
	        #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	        call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    	call ap_hist_mode (Memr[x], Memr[shgm], Memr[w], nbins,
		    sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	    } else
	    	call ap_hist_mode (Memr[x], Memr[hgm], Memr[w], nbins,
		    sky_mode, sky_sigma, sky_skew, maxfit, TOL, ier)
	    sky_mode = max (dmin, min (sky_mode, dmax))
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
pointer	sp, list, fit, nl
real	p[4], dp[4], dummy1
int	locpr()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (list, 4, TY_INT)
	call salloc (fit, nbins, TY_REAL)

	# Initialize.
	do i = 1, 4
	    Memi[list+i-1] = i

	# Compute initial guesses for the parameters.
	call ap_alimr (hgm, nbins, dummy1, p[1], imin, imax)
	p[2] = x[imax]
	#p[3] = max (sky_sigma ** 2, abs (x[2] - x[1]) ** 2)
	p[3] = abs ((x[nbins] - x[1]) / 6.0) ** 2
	p[4] = 0.0
	np = 4

	# Fit the histogram.
	call nlinitr (nl, locpr (gausskew), locpr (dgausskew), p, dp, np,
	    Memi[list], 4, tol, maxiter)
	call nlfitr (nl, x, hgm, w, nbins, 1, WTS_UNIFORM, fier)
	call nlvectorr (nl, x, Memr[fit], nbins, 1)
	call nlpgetr (nl, p, np)
	call nlfreer (nl)

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

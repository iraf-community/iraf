include "../lib/fitsky.h"

define	SFRACTION	.20		# Lucy smoothing length

# AP_CENTROID -- Procedure to find the mode, width and skew of the sky
# distribution by computing the moments of the histogram. 

int procedure ap_centroid (skypix, coords, nskypix, snx, sny,
    k1, hwidth, binsize, smooth, k2, rgrow, maxiter, sky_mode, sky_sigma,
    sky_skew, nsky, nsky_reject)

real	skypix[ARB]			# array of sky pixels
int	coords[ARB]			# array of coordinates
int	nskypix				# the number of sky pixels
int	snx, sny			# the maximum dimensions of sky raster
real	k1				# extent of the histogram in skysigma
real	hwidth				# width of the histogram
real	binsize				# the size of the histogram in sky sigma
int	smooth				# smooth the histogram before fitting
real	k2				# rejection criterion for histogram
real	rgrow				# region growing radius in pixels
int	maxiter				# maximum number of rejection cycles
real	sky_mode			# computed sky value
real	sky_sigma			# computed sigma of the sky pixels
real	sky_skew			# skew of sky pixels
int	nsky				# number of sky pixels used in fit
int	nsky_reject			# number of sky pixels rejected

double	sumpx, sumsqpx, sumcbpx
int	nreject, nbins, nker, ier, i, j
pointer	sp, hgm, shgm, wgt
real	hmin, hmax, dh, locut, hicut
int	ap_grow_hist(), aphgmr()

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
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mode,
	    sky_sigma, sky_skew)
	if (! IS_INDEFR(hwidth)) {
	    hmin = sky_mode - k1 * hwidth
	    hmax = sky_mode + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    hmin = sky_mode - k1 * sky_sigma
	    hmax = sky_mode + k1 * sky_sigma
	    dh = binsize * sky_sigma
	}

	# Compute the number of histogram bins and the size of the smoothing
	# kernel.
	if (dh <= 0.0) {
	    nbins = 1
	    nker = 1
	} else {
	    nbins = 2 * int ((hmax - sky_mode) / dh) + 1
	    nker = 2 * int (SFRACTION * (hmax - sky_mode) / dh) + 1
	}

	# Check for a valid histogram.
	if (nbins < 2 || k1 <= 0.0 || sky_sigma < dh || dh <= 0.0 ||
	    sky_sigma <= 0.0) {
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

	# Do the initial rejection.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
		if (Memr[wgt+i-1] <= 0.0) {
		    sumpx = sumpx - skypix[i]
		    sumsqpx = sumsqpx - skypix[i] ** 2
		    sumcbpx = sumcbpx - skypix[i] ** 3
		}
	    }
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mode, sky_sigma,
	        sky_skew)
	}

	# Fit the mode, sigma an skew of the histogram.
	if (smooth == YES) {
	    call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_imode (Memr[shgm], nbins, hmin, hmax, sky_mode, sky_sigma,
		sky_skew, ier)
	} else
	    call ap_imode (Memr[hgm], nbins, hmin, hmax, sky_mode, sky_sigma,
		sky_skew, ier)
	if (k2 <= 0.0 || sky_sigma <= dh || maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute new histogram limits.
	    locut = sky_mode - k2 * sky_sigma
	    hicut = sky_mode + k2 * sky_sigma

	    # Reject pixels.
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

	    # Recompute the histogram peak.
	    if (smooth == YES) {
	        call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	        call ap_imode (Memr[shgm], nbins, hmin, hmax, sky_mode,
		    sky_sigma, sky_skew, ier)
	    } else
	        call ap_imode (Memr[hgm], nbins, hmin, hmax, sky_mode,
		    sky_sigma, sky_skew, ier)
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

procedure ap_imode (hgm, nbins, z1, z2, sky_mode, sky_sigma, sky_skew, ier)

real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# min and max of the histogram
real	sky_mode		# mode of histogram
real	sky_sigma		# sigma of histogram
real	sky_skew		# skew of histogram
int	ier			# error code

int	i, noccup
real	dz, x, sumi, sumix, sumix2, sumix3

begin
	# Initialize the sums.
	sumi = 0.0
	sumix = 0.0
	sumix2 = 0.0
	sumix3 = 0.0

	# Accumulate the sums.
	noccup = 1
	dz = (z2 - z1) / (nbins - 1)
	x = z1
	do i = 1, nbins {
	    if (hgm[i] > 0.0) {
	        sumi = sumi + hgm[i]
	        sumix = sumix + hgm[i] * x
	        sumix2 = sumix2 + hgm[i] * x * x
	        sumix3 = sumix3 + hgm[i] * x * x * x
	        noccup = noccup + 1
	    }
	    x = x + dz
	}

	# Compute the sky mode, sigma and skew.
	if (sumi > 0.0) {
	    sky_mode = sumix / sumi
	    if (noccup <= 1) {
		sky_sigma = 0.0
		sky_skew = 0.0
	    } else {
	        sky_sigma = sumix2 / sumi - sky_mode ** 2
	        if (sky_sigma <= 0.0) {
		    sky_sigma = 0.0
		    sky_skew = 0.0
	        } else {
		    sky_skew = sumix3 / sumi - 3.0 * sky_sigma * sky_mode -
		        sky_mode ** 3
		    sky_sigma = sqrt (sky_sigma)
		    if (sky_skew <= 0.0)
		        sky_skew = - (abs (sky_skew) ** (1.0 / 3.0))
		    else
		        sky_skew = sky_skew ** (1.0 / 3.0)
	        }
	    }
	    ier = AP_OK
	} else {
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    ier = AP_NOHISTOGRAM
	}
end

include "../lib/fitsky.h"

# AP_CROSSCOR -- Procedure to compute the sky value by calculating the
# cross-correlation function of the histogram of the sky pixels and
# a Gaussian function with the same sigma as the sky distribution.
# The peak of the cross-correlation function is found by parabolic
# interpolation.

int procedure ap_crosscor (skypix, coords, nskypix, snx, sny, k1, hwidth,
	binsize, smooth, k2, rgrow, maxiter, sky_mode, sky_sigma, sky_skew,
	nsky, nsky_reject)

real	skypix[ARB]		# array of sky pixels
int	coords[ARB]		# array of sky coordinates
int	nskypix			# the number of sky pixels
int	snx, sny		# the maximum dimensions of sky raster
real	k1			# half-width of the histogram in sky sigma
real	hwidth			# the input sky sigma
real	binsize			# the size of the histogram in sky sigma
int	smooth			# smooth the histogram before fitting (not used)
real	k2			# rejection limit in skysigma
real	rgrow			# region growing radius in pixels
int	maxiter			# maximum number of rejection cycles
real	sky_mode		# computed sky value
real	sky_sigma		# computed standard deviation of the sky pixels
real	sky_skew		# computed skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of sky pixels rejected

double 	sumpx, sumsqpx, sumcbpx
int	nreject, nbins, nker, nsmooth, ier, i, j
pointer	sp, x, hgm, shgm, wgt, kernel
real	hmin, hmax, dh, locut, hicut, sky_mean
int	ap_grow_hist2(), aphgmr()

begin
	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_NOSKYAREA)

	# Set up initial guess at sky mean, sigma and skew.
	call apfmoments (skypix, nskypix, sumpx, sumsqpx, sumcbpx, sky_mean,
	    sky_sigma, sky_skew)

	# Compute the width and bin size of the histogram.
	if (! IS_INDEFR(hwidth)) {
	    hmin = sky_mean - k1 * hwidth
	    hmax = sky_mean + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    hmin = sky_mean - k1 * sky_sigma
	    hmax = sky_mean + k1 * sky_sigma
	    dh = binsize * sky_sigma
	} 

	# Compute the number of bins in and the width of the kernel.
	if (dh <= 0.0) {
	    nbins = 1
	    nker = 1
	    nsmooth = 1
	} else {
    	    nbins = 2 * int ((hmax - sky_mean) / dh) + 1
	    nker = 2 * int ((hmax - sky_mean) / dh / 2.0) + 1
	    nsmooth = nbins - nker + 1
	}

	# Test for a valid histogram.
	if (nbins < 2 || k1 <= 0.0 || sky_sigma <= 0.0 || dh <= 0.0 ||
	    sky_sigma <= dh) {
	    sky_mode = sky_mean
	    sky_sigma = 0.0
	    sky_skew = 0.0
	    return (AP_NOHISTOGRAM)
	}

	# Allocate space for the histogram and kernel.
	call smark (sp)
	call salloc (x, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)
	call salloc (shgm, nbins, TY_REAL)
	call salloc (wgt, nskypix, TY_REAL)
	call salloc (kernel, nker, TY_REAL)

	# Set up x array.
	do i = 1, nbins
	    Memr[x+i-1] = i
	call amapr (Memr[x], Memr[x], nbins, 1.0, real (nbins), hmin, hmax)

	# Accumulate the histogram.
	call aclrr (Memr[hgm], nbins)
	call aclrr (Memr[shgm], nbins)
	call amovkr (1.0, Memr[wgt], nskypix)
	nsky_reject = nsky_reject + aphgmr (skypix, Memr[wgt], nskypix,
	    Memr[hgm], nbins, hmin, hmax)
	nsky = nskypix - nsky_reject

	# Perform the initial rejection cycle.
	if (nsky_reject > 0.0) {
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

	# Construct kernel and convolve with histogram.
	if (sky_sigma > 0.0) {
	    call ap_gauss_kernel (Memr[kernel], nker, - (hmax - hmin) / 4.0,
	        (hmax - hmin) / 4.0, sky_sigma)
	    call acnvr (Memr[hgm], Memr[shgm+nker/2], nsmooth, Memr[kernel],
	        nker)
	} else
	    call amovr (Memr[hgm], Memr[shgm], nbins)
	call ap_corfit (Memr[x], Memr[shgm], nbins, sky_mode, ier)
	if (ier != OK) {
	    call sfree (sp)
	    return (ier)
	}
	if (k2 <= 0.0 || sky_sigma <= dh || maxiter < 1) {
	    call sfree (sp)
	    return (AP_OK)
	}

	# Fit histogram with pixel rejection and optional region growing.
	do i = 1, maxiter {

	    # Compute new histogram limits.
	    locut = sky_mode - k2 * sky_sigma
	    hicut = sky_mode + k2 * sky_sigma

	    # Detect rejected pixels.
	    nreject = 0
	    do j = 1, nskypix {
		if (skypix[j] >= locut && skypix[j] <= hicut)
		    next
		if (rgrow > 0.0)
		    nreject = nreject + ap_grow_hist2 (skypix, coords,
			Memr[wgt], nskypix, j, snx, sny, Memr[hgm], nbins,
			hmin, hmax, rgrow, sumpx, sumsqpx, sumcbpx)
		else if (Memr[wgt+j-1] > 0.0) {
		    call ap_hgmsub2 (Memr[hgm], nbins, hmin, hmax,
			skypix[j], sumpx, sumsqpx, sumcbpx)
		    Memr[wgt+j-1] = 0.0
		    nreject = nreject + 1
		}
	    }
	    if (nreject == 0)
		break

	    # Update the sky parameters.
	    nsky_reject = nsky_reject + nreject
	    nsky = nskypix - nsky_reject 
	    if (nsky <= 0)
		break
	    call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_mean, sky_sigma,
	        sky_skew)
	    if (sky_sigma <= dh)
		break

	    # Recompute the peak of the histogram.
	    call ap_gauss_kernel (Memr[kernel], nker, - (hmax - hmin) / 4.0,
	        (hmax - hmin) / 4.0, sky_sigma)
	    call aclrr (Memr[shgm], nbins)
	    call acnvr (Memr[hgm], Memr[shgm+nker/2], nsmooth, Memr[kernel],
	        nker)
	    call ap_corfit (Memr[x], Memr[shgm], nbins, sky_mode, ier)
	    if (ier != AP_OK)
		 break
	}

	# Return the appropriate error code.
	call sfree (sp)
	if (nsky == 0 || nsky_reject == nskypix) {
	    nsky = 0
	    nsky_reject = nskypix
	    sky_mode = INDEFR
	    sky_sigma = INDEFR
	    sky_skew = INDEFR
	    return (AP_NSKY_TOO_SMALL)
	} else if (ier != AP_OK) {
	    sky_mode = sky_mean
	    sky_sigma = 0.0
	    sky_skew = 0.0 
	    return (ier)
	} else
	    return (AP_OK)
end


# AP_GAUSS_KERNEL -- Procedure to compute a Gaussian kernel of given length
# and sigma.

procedure ap_gauss_kernel (kernel, nker, kmin, kmax, sky_sigma)

real	kernel[ARB]		# kernel 
int	nker			# length of kernel
real	kmin, kmax		# limits of the kernel
real	sky_sigma		# sigma of the sky

int	i
real	dk, x, sumx

begin
	# Return 1 if unit sized kernel.
	if (nker == 1) {
	    kernel[1] = 1.0
	    return
	}

	# Intialize.
	sumx = 0.0
	x = kmin
	dk = (kmax - kmin ) / (nker - 1)

 	# Compute and normalize the kernel.
	do i = 1, nker {
	    kernel[i] = exp (- (x ** 2) / (2. * sky_sigma ** 2))
	    sumx = sumx + kernel[i]
	    x = x + dk
	}
        do i = 1, nker
            kernel[i] = kernel[i] / sumx
end


# AP_CORFIT -- Procedure to compute the peak of the cross-correlation
# function using parabolic interpolation.

procedure ap_corfit (x, shgm, nbins, sky_mode, ier)

real	x[ARB]				# x coordinates of histogram
real	shgm[ARB]			# convolved histogram
int	nbins				# number of histogram bins
real	sky_mode			# computed sky_mode
int	ier				# error code

int	bin
real	max, xo, dh1, dh2

begin
	call ap_amaxel (shgm, nbins, max, bin)
	if (max <= 0) {
	    ier = AP_FLAT_HIST
	} else if (bin == 1) {
	    sky_mode = x[1]
	    ier = AP_OK
	} else if (bin == nbins) {
	    sky_mode = x[nbins]
	    ier = AP_OK
	} else {
	    xo = 0.5 * (x[bin] + x[bin-1])
	    dh1 = shgm[bin] - shgm[bin-1] 
	    dh2 = shgm[bin] - shgm[bin+1]
	    sky_mode = xo + (x[bin] - x[bin-1]) * (dh1 / (dh1 + dh2))
	    ier = AP_OK
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<imio.h>

# User callable routines.
# ZSCALE -- Sample an image and compute greyscale limits.
# MZSCALE -- Sample an image with pixel masks and compute greyscale limits.
# ZSC_PMSECTION -- Create a pixel mask from an image section.
# ZSC_ZLIMITS -- Compute Z transform limits from a sample of pixels.


# ZSCALE -- Sample an image and compute greyscale limits.
# A sample mask is created based on the input parameters and then
# MZSCALE is called.

procedure zscale (im, z1, z2, contrast, optimal_sample_size, len_stdline)

pointer	im			# image to be sampled
real	z1, z2			# output min and max greyscale values
real	contrast		# adj. to slope of transfer function
int	optimal_sample_size	# desired number of pixels in sample
int	len_stdline		# optimal number of pixels per line

int	nc, nl
pointer	sp, section, zpm, zsc_pmsection()
errchk	zsc_pmsection, mzscale

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)

	# Make the sample image section.
	switch (IM_NDIM(im)) {
	case 1:
	    call sprintf (Memc[section], SZ_FNAME, "[*]")
	default:
	    nc = max (1, min (IM_LEN(im,1), len_stdline))
	    nl = max (1, min (IM_LEN(im,2), optimal_sample_size / nc))
	    call sprintf (Memc[section], SZ_FNAME, "[*:%d,*:%d]")
		call pargi (IM_LEN(im,1) / nc)
		call pargi (IM_LEN(im,2) / nl)
	}

	# Make a mask and compute the greyscale limits.
	zpm = zsc_pmsection (Memc[section], im)
	call mzscale (im, zpm, NULL, contrast, optimal_sample_size, z1, z2)
	call imunmap (zpm)
	call sfree (sp)
end


# MZSCALE -- Sample an image with pixel masks and compute greyscale limits.
# The image is sampled through a pixel mask.  If no pixel mask is given
# a uniform sample mask is generated.  If a bad pixel mask is given
# bad pixels in the sample are eliminated.  Once the sample is obtained
# the greyscale limits are obtained using the ZSC_ZLIMITS algorithm.

procedure mzscale (im, zpm, bpm, contrast, maxpix, z1, z2)

pointer	im			#I image to be sampled
pointer	zpm			#I pixel mask for sampling
pointer	bpm			#I bad pixel mask
real	contrast		#I contrast parameter
int	maxpix			#I maximum number of pixels in sample
real	z1, z2			#O output min and max greyscale values

int	i, ndim, nc, nl, npix, nbp, imstati()
pointer	sp, section, v, sample, zmask, bp, zim, pmz, pmb, buf
pointer	zsc_pmsection(), imgnlr()
bool	pm_linenotempty()
errchk	zsc_pmsection, zsc_zlimits

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (sample, maxpix, TY_REAL)
	zmask = NULL
	bp = NULL

	ndim = min (2, IM_NDIM(im))
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	# Generate a uniform sample mask if none is given.
	if (zpm == NULL) {
	    switch (IM_NDIM(im)) {
	    case 1:
		call sprintf (Memc[section], SZ_FNAME, "[*]")
	    default:
		i = max (1., sqrt ((nc-1)*(nl-1) / real (maxpix)))
		call sprintf (Memc[section], SZ_FNAME, "[*:%d,*:%d]")
		    call pargi (i)
		    call pargi (i)
	    }
	    zim = zsc_pmsection (Memc[section], im)
	    pmz = imstati (zim, IM_PMDES)
	} else
	    pmz = imstati (zpm, IM_PMDES)

	# Set bad pixel mask.
	if (bpm != NULL)
	    pmb = imstati (bpm, IM_PMDES)
	else
	    pmb = NULL

	# Get the sample up to maxpix pixels.
	npix = 0
	nbp = 0
	call amovkl (long(1), Memi[v], IM_MAXDIM)
	repeat {
	    if (pm_linenotempty (pmz, Meml[v])) {
		if (zmask == NULL)
		    call salloc (zmask, nc, TY_INT)
		call pmglpi (pmz, Meml[v], Memi[zmask], 0, nc, 0)
		if (pmb != NULL) {
		    if (pm_linenotempty (pmb, Meml[v])) {
			if (bp == NULL)
			    call salloc (bp, nc, TY_INT)
			call pmglpi (pmb, Meml[v], Memi[bp], 0, nc, 0)
			nbp = nc
		    } else
			nbp = 0

		}
		if (imgnlr (im, buf, Meml[v]) == EOF)
		    break
		do i = 0, nc-1 {
		    if (Memi[zmask+i] == 0)
			next
		    if (nbp > 0)
			if (Memi[bp+i] != 0)
			    next
		    Memr[sample+npix] = Memr[buf+i]
		    npix = npix + 1
		    if (npix == maxpix)
			break
		}
		if (npix == maxpix)
		    break
	    } else {
		do i = 2, ndim {
		    Meml[v+i-1] = Meml[v+i-1] + 1
		    if (Meml[v+i-1] <= IM_LEN(im,i))
			break
		    else if (i < ndim)
			Meml[v+i-1] = 1
		}
	    }
        } until (Meml[v+ndim-1] > IM_LEN(im,ndim))

	if (zpm == NULL)
	    call imunmap (zim)

	# Compute greyscale limits.
	call zsc_zlimits (Memr[sample], npix, contrast, z1, z2)

	call sfree (sp)
end


# ZSC_PMSECTION -- Create a pixel mask from an image section.
# This only applies the mask to the first plane of the image.

pointer procedure zsc_pmsection (section, refim)

char	section[ARB]		#I Image section
pointer	refim			#I Reference image pointer

int	i, j, ip, ndim, temp, a[2], b[2], c[2], rop, ctoi()
pointer	pm, im, mw, dummy, pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
define  error_  99

begin
        # Decode the section string.
	call amovki (1, a, 2)
	call amovki (1, b, 2)
	call amovki (1, c, 2)
	ndim = min (2, IM_NDIM(refim))
	do i = 1, ndim
	    b[i] = IM_LEN(refim,i)

        ip = 1
        while (IS_WHITE(section[ip]))
            ip = ip + 1
        if (section[ip] == '[') {
            ip = ip + 1

	    do i = 1, ndim {
		while (IS_WHITE(section[ip]))
		    ip = ip + 1

		# Get a:b:c.  Allow notation such as "-*:c"
		# (or even "-:c") where the step is obviously negative.

		if (ctoi (section, ip, temp) > 0) {                 # a
		    a[i] = temp
		    if (section[ip] == ':') {
			ip = ip + 1
			if (ctoi (section, ip, b[i]) == 0)             # a:b
			    goto error_
		    } else
			b[i] = a[i]
		} else if (section[ip] == '-') {                    # -*
		    temp = a[i]
		    a[i] = b[i]
		    b[i] = temp
		    ip = ip + 1
		    if (section[ip] == '*')
			ip = ip + 1
		} else if (section[ip] == '*')                      # *
		    ip = ip + 1
		if (section[ip] == ':') {                           # ..:step
		    ip = ip + 1
		    if (ctoi (section, ip, c[i]) == 0)
			goto error_
		    else if (c[i] == 0)
			goto error_
		}
		if (a[i] > b[i] && c[i] > 0)
		    c[i] = -c[i]

		while (IS_WHITE(section[ip]))
		    ip = ip + 1
		if (i < ndim) {
		    if (section[ip] != ',')
			goto error_
		} else {
		    if (section[ip] != ']')
			goto error_
		}
		ip = ip + 1
	    }
	}

	# In this case make the values be increasing only.
	do i = 1, ndim
	    if (c[i] < 0) {
		temp = a[i]
		a[i] = b[i]
		b[i] = temp
		c[i] = -c[i]
	    }

	# Make the mask.
	pm = pm_newmask (refim, 16)

	rop = PIX_SET+PIX_VALUE(1)
	if (c[1] == 1 && c[2] == 1)
	    call pm_box (pm, a[1], a[2], b[1], b[2], rop)

	else if (c[1] == 1)
	    for (i=a[2]; i<=b[2]; i=i+c[2])
		call pm_box (pm, a[1], i, b[1], i, rop)

	else
	    for (i=a[2]; i<=b[2]; i=i+c[2])
		for (j=a[1]; j<=b[1]; j=j+c[1])
		    call pm_point (pm, j, i, rop)

	i = IM_NPHYSDIM(refim)
	IM_NPHYSDIM(refim) = ndim
	im = im_pmmapo (pm, refim)
	IM_NPHYSDIM(refim) = i
	dummy = imgl1i (im)		# Force I/O to set header
	ifnoerr (mw = mw_openim (refim)) {		# Set WCS
	    call mw_saveim (mw, im)
	    call mw_close (mw)
	}

	return (im)

error_
        call error (1, "Error in image section specification")
end


.help zsc_zlimits
.nf ___________________________________________________________________________
ZSC_ZLIMITS -- Compute limits for a linear transform that best samples the
the histogram about the median value.  This is often called to compute
greyscale limits from a sample of pixel values.

If the number of pixels is too small an error condition is returned.  If
the contrast parameter value is zero the limits of the sample are
returned.  Otherwise the sample is sorted and the median is found from the
central value(s).  A straight line is fitted to the sorted sample with
interative rejection.  If more than half the pixels are rejected the full
range is returned.  The contrast parameter is used to adjust the transfer
slope about the median.  The final limits are the extension of the fitted
line to the first and last array index.
.endhelp ______________________________________________________________________

define	MIN_NPIXELS	5		# smallest permissible sample
define	MAX_REJECT	0.5		# max frac. of pixels to be rejected
define	GOOD_PIXEL	0		# use pixel in fit
define	BAD_PIXEL	1		# ignore pixel in all computations
define	REJECT_PIXEL	2		# reject pixel after a bit
define	KREJ		2.5		# k-sigma pixel rejection factor
define	MAX_ITERATIONS	5		# maximum number of fitline iterations


# ZSC_ZLIMITS -- Compute Z transform limits from a sample of pixels.

procedure zsc_zlimits (sample, npix, contrast, z1, z2)

real	sample[ARB]	#I Sample of pixel values (possibly resorted)
int	npix		#I Number of pixels
real	contrast	#I Contrast algorithm parameter
real	z1, z2		#O Z transform limits

int	center_pixel, minpix, ngoodpix, ngrow, zsc_fit_line()
real	zmin, zmax, median
real	zstart, zslope

begin
	# Check for a sufficient sample.
	if (npix < MIN_NPIXELS)
	    call error (1, "Insufficient sample pixels found")

	# If contrast is zero return the range.
	if (contrast == 0.) {
	    call alimr (sample, npix, z1, z2)
	    return
	}
	    
	# Sort the sample, compute the range, and median pixel values.
	# The median value is the average of the two central values if there 
	# are an even number of pixels in the sample.

	call asrtr (sample, sample, npix)
	zmin = sample[1]
	zmax = sample[npix]

	center_pixel = (npix + 1) / 2
	if (mod (npix, 2) == 1)
	    median = sample[center_pixel]
	else
	    median = (sample[center_pixel] + sample[center_pixel+1]) / 2

	# Fit a line to the sorted sample vector.  If more than half of the
	# pixels in the sample are rejected give up and return the full range.
	# If the user-supplied contrast factor is not 1.0 adjust the scale
	# accordingly and compute Z1 and Z2, the y intercepts at indices 1 and
	# npix.

	minpix = max (MIN_NPIXELS, int (npix * MAX_REJECT))
	ngrow = max (1, nint (npix * .01))
	ngoodpix = zsc_fit_line (sample, npix, zstart, zslope,
	    KREJ, ngrow, MAX_ITERATIONS)

	if (ngoodpix < minpix) {
	    z1 = zmin
	    z2 = zmax
	} else {
	    if (contrast > 0)
		zslope = zslope / contrast
	    z1 = max (zmin, median - (center_pixel - 1) * zslope)
	    z2 = min (zmax, median + (npix - center_pixel) * zslope)
	}
end


# ZSC_FIT_LINE -- Fit a straight line to a data array of type real.  This is
# an iterative fitting algorithm, wherein points further than ksigma from the
# current fit are excluded from the next fit.  Convergence occurs when the
# next iteration does not decrease the number of pixels in the fit, or when
# there are no pixels left.  The number of pixels left after pixel rejection
# is returned as the function value.

int procedure zsc_fit_line (data, npix, zstart, zslope, krej, ngrow, maxiter)

real	data[npix]		# data to be fitted
int	npix			# number of pixels before rejection
real	zstart			# Z-value of pixel data[1]	(output)
real	zslope			# dz/pixel			(output)
real	krej			# k-sigma pixel rejection factor
int	ngrow			# number of pixels of growing
int	maxiter			# max iterations

int	i, ngoodpix, last_ngoodpix, minpix, niter
real	xscale, z0, dz, x, z, mean, sigma, threshold
double	sumxsqr, sumxz, sumz, sumx, rowrat
pointer	sp, flat, badpix, normx
int	zsc_reject_pixels(), zsc_compute_sigma()

begin
	call smark (sp)

	if (npix <= 0)
	    return (0)
	else if (npix == 1) {
	    zstart = data[1]
	    zslope = 0.0
	    return (1)
	} else
	    xscale = 2.0 / (npix - 1)

	# Allocate a buffer for data minus fitted curve, another for the
	# normalized X values, and another to flag rejected pixels.

	call salloc (flat, npix, TY_REAL)
	call salloc (normx, npix, TY_REAL)
	call salloc (badpix, npix, TY_SHORT)
	call aclrs (Mems[badpix], npix)

	# Compute normalized X vector.  The data X values [1:npix] are
	# normalized to the range [-1:1].  This diagonalizes the lsq matrix
	# and reduces its condition number.

	do i = 0, npix - 1
	    Memr[normx+i] = i * xscale - 1.0

	# Fit a line with no pixel rejection.  Accumulate the elements of the
	# matrix and data vector.  The matrix M is diagonal with
	# M[1,1] = sum x**2 and M[2,2] = ngoodpix.  The data vector is
	# DV[1] = sum (data[i] * x[i]) and DV[2] = sum (data[i]).

	sumxsqr = 0
	sumxz = 0
	sumx = 0
	sumz = 0

	do i = 1, npix {
	    x = Memr[normx+i-1]
	    z = data[i]
	    sumxsqr = sumxsqr + (x ** 2)
	    sumxz   = sumxz + z * x
	    sumz    = sumz + z
	}

	# Solve for the coefficients of the fitted line.
	z0 = sumz / npix
	dz = sumxz / sumxsqr

	# Iterate, fitting a new line in each iteration.  Compute the flattened
	# data vector and the sigma of the flat vector.  Compute the lower and
	# upper k-sigma pixel rejection thresholds.  Run down the flat array
	# and detect pixels to be rejected from the fit.  Reject pixels from
	# the fit by subtracting their contributions from the matrix sums and
	# marking the pixel as rejected.

	ngoodpix = npix
	minpix = max (MIN_NPIXELS, int (npix * MAX_REJECT))

	for (niter=1;  niter <= maxiter;  niter=niter+1) {
	    last_ngoodpix = ngoodpix

	    # Subtract the fitted line from the data array.
	    call zsc_flatten_data (data, Memr[flat], Memr[normx], npix, z0, dz)

	    # Compute the k-sigma rejection threshold.  In principle this
	    # could be more efficiently computed using the matrix sums
	    # accumulated when the line was fitted, but there are problems with
	    # numerical stability with that approach.

	    ngoodpix = zsc_compute_sigma (Memr[flat], Mems[badpix], npix,
		mean, sigma)
	    threshold = sigma * krej

	    # Detect and reject pixels further than ksigma from the fitted
	    # line.
	    ngoodpix = zsc_reject_pixels (data, Memr[flat], Memr[normx],
		Mems[badpix], npix, sumxsqr, sumxz, sumx, sumz, threshold,
		ngrow)

	    # Solve for the coefficients of the fitted line.  Note that after
	    # pixel rejection the sum of the X values need no longer be zero.

	    if (ngoodpix > 0) {
		rowrat = sumx / sumxsqr
		z0 = (sumz - rowrat * sumxz) / (ngoodpix - rowrat * sumx)
		dz = (sumxz - z0 * sumx) / sumxsqr
	    }

	    if (ngoodpix >= last_ngoodpix || ngoodpix < minpix)
		break
	}

	# Transform the line coefficients back to the X range [1:npix].
	zstart = z0 - dz
	zslope = dz * xscale

	call sfree (sp)
	return (ngoodpix)
end


# ZSC_FLATTEN_DATA -- Compute and subtract the fitted line from the data array,
# returned the flattened data in FLAT.

procedure zsc_flatten_data (data, flat, x, npix, z0, dz)

real	data[npix]		# raw data array
real	flat[npix]		# flattened data  (output)
real	x[npix]			# x value of each pixel
int	npix			# number of pixels
real	z0, dz			# z-intercept, dz/dx of fitted line
int	i

begin
	do i = 1, npix
	    flat[i] = data[i] - (x[i] * dz + z0)
end


# ZSC_COMPUTE_SIGMA -- Compute the root mean square deviation from the
# mean of a flattened array.  Ignore rejected pixels.

int procedure zsc_compute_sigma (a, badpix, npix, mean, sigma)

real	a[npix]			# flattened data array
short	badpix[npix]		# bad pixel flags (!= 0 if bad pixel)
int	npix
real	mean, sigma		# (output)

real	pixval
int	i, ngoodpix
double	sum, sumsq, temp

begin
	sum = 0
	sumsq = 0
	ngoodpix = 0

	# Accumulate sum and sum of squares.
	do i = 1, npix
	    if (badpix[i] == GOOD_PIXEL) {
		pixval = a[i]
		ngoodpix = ngoodpix + 1
		sum = sum + pixval
		sumsq = sumsq + pixval ** 2
	    }

	# Compute mean and sigma.
	switch (ngoodpix) {
	case 0:
	    mean = INDEF
	    sigma = INDEF
	case 1:
	    mean = sum
	    sigma = INDEF
	default:
	    mean = sum / ngoodpix
	    temp = sumsq / (ngoodpix - 1) - sum**2 / (ngoodpix * (ngoodpix - 1))
	    if (temp < 0)		# possible with roundoff error
		sigma = 0.0
	    else
		sigma = sqrt (temp)
	}

	return (ngoodpix)
end


# ZSC_REJECT_PIXELS -- Detect and reject pixels more than "threshold" greyscale
# units from the fitted line.  The residuals about the fitted line are given
# by the "flat" array, while the raw data is in "data".  Each time a pixel
# is rejected subtract its contributions from the matrix sums and flag the
# pixel as rejected.  When a pixel is rejected reject its neighbors out to
# a specified radius as well.  This speeds up convergence considerably and
# produces a more stringent rejection criteria which takes advantage of the
# fact that bad pixels tend to be clumped.  The number of pixels left in the
# fit is returned as the function value.

int procedure zsc_reject_pixels (data, flat, normx, badpix, npix,
				 sumxsqr, sumxz, sumx, sumz, threshold, ngrow)

real	data[npix]		# raw data array
real	flat[npix]		# flattened data array
real	normx[npix]		# normalized x values of pixels
short	badpix[npix]		# bad pixel flags (!= 0 if bad pixel)
int	npix
double	sumxsqr,sumxz,sumx,sumz	# matrix sums
real	threshold		# threshold for pixel rejection
int	ngrow			# number of pixels of growing

int	ngoodpix, i, j
real	residual, lcut, hcut
double	x, z

begin
	ngoodpix = npix
	lcut = -threshold
	hcut = threshold

	do i = 1, npix
	    if (badpix[i] == BAD_PIXEL)
		ngoodpix = ngoodpix - 1
	    else {
		residual = flat[i]
		if (residual < lcut || residual > hcut) {
		    # Reject the pixel and its neighbors out to the growing
		    # radius.  We must be careful how we do this to avoid
		    # directional effects.  Do not turn off thresholding on
		    # pixels in the forward direction; mark them for rejection
		    # but do not reject until they have been thresholded.
		    # If this is not done growing will not be symmetric.

		    do j = max(1,i-ngrow), min(npix,i+ngrow) {
			if (badpix[j] != BAD_PIXEL) {
			    if (j <= i) {
				x = normx[j]
				z = data[j]
				sumxsqr = sumxsqr - (x ** 2)
				sumxz = sumxz - z * x
				sumx = sumx - x
				sumz = sumz - z
				badpix[j] = BAD_PIXEL
				ngoodpix = ngoodpix - 1
			    } else
				badpix[j] = REJECT_PIXEL
			}
		    }
		}
	    }

	return (ngoodpix)
end

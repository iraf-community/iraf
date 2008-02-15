# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help zscale
.nf ___________________________________________________________________________
ZSCALE -- Compute the optimal Z1, Z2 (range of greyscale values to be
displayed) of an image.  For efficiency a statistical subsample of an image
is used.  The pixel sample evenly subsamples the image in x and y.  The entire
image is used if the number of pixels in the image is smaller than the desired
sample.

The sample is accumulated in a buffer and sorted by greyscale value.
The median value is the central value of the sorted array.  The slope of a
straight line fitted to the sorted sample is a measure of the standard
deviation of the sample about the median value.  Our algorithm is to sort
the sample and perform an iterative fit of a straight line to the sample,
using pixel rejection to omit gross deviants near the endpoints.  The fitted
straight line is the transfer function used to map image Z into display Z.
If more than half the pixels are rejected the full range is used.  The slope
of the fitted line is divided by the user-supplied contrast factor and the
final Z1 and Z2 are computed, taking the origin of the fitted line at the
median value.
.endhelp ______________________________________________________________________

define	MIN_NPIXELS	5		# smallest permissible sample
define	MAX_REJECT	0.5		# max frac. of pixels to be rejected
define	GOOD_PIXEL	0		# use pixel in fit
define	BAD_PIXEL	1		# ignore pixel in all computations
define	REJECT_PIXEL	2		# reject pixel after a bit
define	KREJ		2.5		# k-sigma pixel rejection factor
define	MAX_ITERATIONS	5		# maximum number of fitline iterations


# ZSCALE -- Sample the image and compute Z1 and Z2.

procedure zscale (im, z1, z2, contrast, optimal_sample_size, len_stdline)

pointer	im			# image to be sampled
real	z1, z2			# output min and max greyscale values
real	contrast		# adj. to slope of transfer function
int	optimal_sample_size	# desired number of pixels in sample
int	len_stdline		# optimal number of pixels per line

int	npix, minpix, ngoodpix, center_pixel, ngrow
real	zmin, zmax, median
real	zstart, zslope
pointer	sample, left
int	zsc_sample_image(), zsc_fit_line()

begin
	# Subsample the image.
	npix = zsc_sample_image (im, sample, optimal_sample_size, len_stdline)
	center_pixel = max (1, (npix + 1) / 2)

	# Sort the sample, compute the minimum, maximum, and median pixel
	# values.

	call asrtr (Memr[sample], Memr[sample], npix)
	zmin = Memr[sample]
	zmax = Memr[sample+npix-1]

	# The median value is the average of the two central values if there 
	# are an even number of pixels in the sample.

	left = sample + center_pixel - 1
	if (mod (npix, 2) == 1 || center_pixel >= npix)
	    median = Memr[left]
	else
	    median = (Memr[left] + Memr[left+1]) / 2

	# Fit a line to the sorted sample vector.  If more than half of the
	# pixels in the sample are rejected give up and return the full range.
	# If the user-supplied contrast factor is not 1.0 adjust the scale
	# accordingly and compute Z1 and Z2, the y intercepts at indices 1 and
	# npix.

	minpix = max (MIN_NPIXELS, int (npix * MAX_REJECT))
	ngrow = max (1, nint (npix * .01))
	ngoodpix = zsc_fit_line (Memr[sample], npix, zstart, zslope,
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

	call mfree (sample, TY_REAL)
end


# ZSC_SAMPLE_IMAGE -- Extract an evenly gridded subsample of the pixels from
# a two-dimensional image into a one-dimensional vector.

int procedure zsc_sample_image (im, sample, optimal_sample_size, len_stdline)

pointer	im			# image to be sampled
pointer	sample			# output vector containing the sample
int	optimal_sample_size	# desired number of pixels in sample
int	len_stdline		# optimal number of pixels per line

int	ncols, nlines, col_step, line_step, maxpix, line
int	opt_npix_per_line, npix_per_line
int	opt_nlines_in_sample, min_nlines_in_sample, max_nlines_in_sample
pointer	op
pointer	imgl2r()

begin
	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Compute the number of pixels each line will contribute to the sample,
	# and the subsampling step size for a line.  The sampling grid must
	# span the whole line on a uniform grid.

	opt_npix_per_line = min (ncols, len_stdline)
	col_step = (ncols + opt_npix_per_line-1) / opt_npix_per_line
	npix_per_line = (ncols + col_step-1) / col_step

	# Compute the number of lines to sample and the spacing between lines.
	# We must ensure that the image is adequately sampled despite its
	# size, hence there is a lower limit on the number of lines in the
	# sample.  We also want to minimize the number of lines accessed when
	# accessing a large image, because each disk seek and read is expensive.
	# The number of lines extracted will be roughly the sample size divided
	# by len_stdline, possibly more if the lines are very short.

	min_nlines_in_sample = max (1, optimal_sample_size / len_stdline)
	opt_nlines_in_sample = max(min_nlines_in_sample, min(nlines,
	    (optimal_sample_size + npix_per_line-1) / npix_per_line))
	line_step = max (1, nlines / (opt_nlines_in_sample))
	max_nlines_in_sample = (nlines + line_step-1) / line_step

	# Allocate space for the output vector.  Buffer must be freed by our
	# caller.

	maxpix = npix_per_line * max_nlines_in_sample
	call malloc (sample, maxpix, TY_REAL)

# call eprintf ("sample: x[%d:%d:%d] y[%d:%d:%d]\n")
# call pargi(1);call pargi(ncols); call pargi(col_step)
# call pargi((line_step+1)/2); call pargi(nlines); call pargi(line_step)

	# Extract the vector.
	op = sample
	do line = (line_step + 1) / 2, nlines, line_step {
	    call zsc_subsample (Memr[imgl2r(im,line)], Memr[op],
		npix_per_line, col_step)
	    op = op + npix_per_line
	    if (op - sample + npix_per_line > maxpix)
		break
	}

	return (op - sample)
end


# ZSC_SUBSAMPLE -- Subsample an image line.  Extract the first pixel and
# every "step"th pixel thereafter for a total of npix pixels.

procedure zsc_subsample (a, b, npix, step)

real	a[ARB]
real	b[npix]
int	npix, step
int	ip, i

begin
	if (step <= 1)
	    call amovr (a, b, npix)
	else {
	    ip = 1
	    do i = 1, npix {
		b[i] = a[ip]
		ip = ip + step
	    }
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
# call eprintf ("\t%10g %10g %10g\n")
# call pargd(sumxsqr); call pargd(sumxz); call pargd(sumz)

	# Solve for the coefficients of the fitted line.
	z0 = sumz / npix
	dz = sumxz / sumxsqr

# call eprintf ("fit: z0=%g, dz=%g\n")
# call pargr(z0); call pargr(dz)

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

# call eprintf ("fit: z0=%g, dz=%g, threshold=%g, npix=%d\n")
# call pargr(z0); call pargr(dz); call pargr(threshold); call pargi(ngoodpix)

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
#call eprintf ("\t\t%d->%d\tcheck\n");call pargi(j); call pargs(badpix[j])
			if (badpix[j] != BAD_PIXEL) {
			    if (j <= i) {
				x = normx[j]
				z = data[j]
#call eprintf ("\treject [%d:%6g]=%6g sum[xsqr,xz,z]\n")
#call pargi(j); call pargd(x); call pargd(z)
#call eprintf ("\t%10g %10g %10g\n")
#call pargd(sumxsqr); call pargd(sumxz); call pargd(sumz)
				sumxsqr = sumxsqr - (x ** 2)
				sumxz = sumxz - z * x
				sumx = sumx - x
				sumz = sumz - z
#call eprintf ("\t%10g %10g %10g\n")
#call pargd(sumxsqr); call pargd(sumxz); call pargd(sumz)
				badpix[j] = BAD_PIXEL
				ngoodpix = ngoodpix - 1
			    } else
				badpix[j] = REJECT_PIXEL
#call eprintf ("\t\t%d->%d\tset\n");call pargi(j); call pargs(badpix[j])
			}
		    }
		}
	    }

	return (ngoodpix)
end

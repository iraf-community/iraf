
include <math.h>

define	FWHM_TO_SIGMA	0.42467
define	RMIN		2.01

# AP_EGPARAMS -- Calculate the parameters of the elliptical Gaussian needed
# to compute the kernel.

procedure ap_egparams (sigma, ratio, theta, nsigma, a, b, c, f, nx, ny)

real	sigma			# sigma of Gaussian in x
real	ratio			# Ratio of half-width in y to x
real	theta			# position angle of Gaussian
real	nsigma			# limit of convolution
real	a, b, c, f		# ellipse parameters
int	nx, ny			# dimensions of the kernel

real	sx2, sy2, cost, sint, discrim
bool	fp_equalr ()

begin
	# Define some temporary variables.
	sx2 = sigma ** 2
	sy2 = (ratio * sigma) ** 2
	cost = cos (DEGTORAD (theta))
	sint = sin (DEGTORAD (theta))

	# Compute the ellipse parameters.
	if (fp_equalr (ratio, 0.0)) {
	    if (fp_equalr (theta, 0.0) || fp_equalr (theta, 180.)) {
		a = 1. / sx2
		b = 0.0
		c = 0.0
	    } else if (fp_equalr (theta, 90.0)) {
		a = 0.0
		b = 0.0
		c = 1. / sx2
	    } else
		call error (0, "AP_EGPARAMS: Cannot make 1D Gaussian.")
	    f = nsigma ** 2 / 2.
	    nx = 2. * sigma * nsigma * abs (cost) + 1.
	    ny = 2. * sigma * nsigma * abs (sint) + 1.
	} else {
	    a = cost ** 2 / sx2 + sint ** 2 / sy2
	    b = 2. * (1.0 / sx2 - 1.0 / sy2) * cost * sint
	    c = sint ** 2 / sx2 + cost ** 2 / sy2
	    discrim = b ** 2 - 4. * a * c
	    f = nsigma ** 2 / 2.
	    nx = 2. * sqrt (-8. * c * f / discrim) + 1.
	    ny = 2. * sqrt (-8. * a * f / discrim) + 1.
	}

	# Correct the f factor if necessary

	# Force the kernel to the nearest odd integer.
	nx = max (5, nx)
	if (mod (nx, 2) == 0)
	    nx = nx + 1
	ny = max (5, ny)
	if (mod (ny, 2) == 0)
	    ny = ny + 1
end


# AP_EGKERNEL -- Compute the elliptical Gaussian kernel.

real procedure ap_egkernel (kernel, skip, nx, ny, a, b, c, f)

real	kernel[nx,ny]		# Gaussian kernel
int	skip[nx,ny]		# Skip subraster
int	nx, ny			# dimensions of the kernel
real	a, b, c, f		# Ellipse parameters

int	i, j, x0, y0, x, y, npts
real	rjsq, rsq, sumk, sumksq, relerr

begin
	# Initialize.
	x0 = nx / 2 + 1
	y0 = ny / 2 + 1
	sumk = 0.0
	sumksq = 0.0
	npts = 0

	# Compute the kernel.
	do j = 1, ny {
	    y = j - y0
	    rjsq = y ** 2
	    do i = 1, nx {
		x = i - x0
		rsq = sqrt (x ** 2 + rjsq)
		kernel[i,j] = 0.5 * (a * x ** 2 + c * y ** 2 + b * x * y)
		if (kernel[i,j] <= f || rsq < RMIN) {
		    kernel[i,j] = exp (-kernel[i,j])
		    sumk = sumk + kernel[i,j]
		    sumksq = sumksq + kernel[i,j] ** 2
		    skip[i,j] = NO
		    npts = npts + 1
		} else {
		    kernel[i,j] = 0.0
		    skip[i,j] = YES
		}
	    }
	}

	# Normalize the kernel.
	sumksq = sumksq - sumk ** 2 / npts
	sumk = sumk / npts
	relerr = 0.0
	do j = 1, ny {
	    do i = 1, nx {
		if (skip[i,j] == NO) {
		    kernel[i,j] = (kernel[i,j] - sumk) / sumksq
		    relerr = relerr + kernel[i,j] ** 2
		}
	    }
	}

	return (sqrt (relerr))
end


# AP_GKERNEL -- Compute the 1D Gaussian convolution kernel.

procedure ap_gkernel (ker1, nbox, fwhmpsf, nsigma)

real	ker1[nbox]		# 1-D kernel
int	nbox			# dimensions of kernel
real	fwhmpsf			# fwhmpsf Gaussian
real	nsigma			# number of sigma in convolution

int	j, middle, jsq
real	sumk1, sumk1sq, sigsq

begin
	# Define constants and zero the accumulators.
	sigsq = (FWHM_TO_SIGMA * fwhmpsf) ** 2
	sumk1 = 0.0
	sumk1sq = 0.0
	middle = nbox / 2 + 1

	# Compute the 1D kernel.
	do j = 1, nbox {
	    jsq = (j - middle) ** 2
	    ker1[j] = exp (-0.5 * jsq / sigsq)
	    sumk1 = sumk1 + ker1[j]
	    sumk1sq = sumk1sq + ker1[j] ** 2
	}

	# Normalize the kernel.
	sumk1sq = sumk1sq - sumk1 ** 2 / nbox
	sumk1 = sumk1 / nbox
	do j = 1, nbox
	    ker1[j] = (ker1[j] - sumk1) / sumk1sq
end

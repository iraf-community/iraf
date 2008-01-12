include <math.h>
include "../lib/find.h"

# Set up the gaussian fitting structure. 

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
	    nx = 2 * int (max (sigma * nsigma * abs (cost), RMIN)) + 1
	    ny = 2 * int (max (sigma * nsigma * abs (sint), RMIN)) + 1
	} else {
	    a = cost ** 2 / sx2 + sint ** 2 / sy2
	    b = 2. * (1.0 / sx2 - 1.0 / sy2) * cost * sint
	    c = sint ** 2 / sx2 + cost ** 2 / sy2
	    discrim = b ** 2 - 4. * a * c
	    f = nsigma ** 2 / 2.
	    nx = 2 * int (max (sqrt (-8. * c * f / discrim), RMIN)) + 1
	    ny = 2 * int (max (sqrt (-8. * a * f / discrim), RMIN)) + 1
	}
end


# AP_EGKERNEL -- Compute the elliptical Gaussian kernel.

real procedure ap_egkernel (gkernel, ngkernel, dkernel, skip, nx, ny, gsums, a,
	b, c, f)

real	gkernel[nx,ny]		# output Gaussian amplitude kernel
real	ngkernel[nx,ny]		# output normalized Gaussian amplitude kernel
real	dkernel[nx,ny]		# output Gaussian sky kernel
int	skip[nx,ny]		# output skip subraster
int	nx, ny			# input dimensions of the kernel
real	gsums[ARB]		# output array of gsums
real	a, b, c, f		# ellipse parameters

int	i, j, x0, y0, x, y
real	npts, rjsq, rsq, relerr,ef

begin
	# Initialize.
	x0 = nx / 2 + 1
	y0 = ny / 2 + 1
	gsums[GAUSS_SUMG] = 0.0
	gsums[GAUSS_SUMGSQ] = 0.0
	npts = 0.0

	# Compute the kernel and principal sums.
	do j = 1, ny {
	    y = j - y0
	    rjsq = y ** 2
	    do i = 1, nx {
		x = i - x0
		rsq = sqrt (x ** 2 + rjsq)
		ef = 0.5 * (a * x ** 2 + c * y ** 2 + b * x * y)
		gkernel[i,j] = exp (-1.0 * ef)
		if (ef <= f || rsq <= RMIN) {
		    #gkernel[i,j] = exp (-ef)
		    ngkernel[i,j] = gkernel[i,j]
		    dkernel[i,j] = 1.0
		    gsums[GAUSS_SUMG] = gsums[GAUSS_SUMG] + gkernel[i,j]
		    gsums[GAUSS_SUMGSQ] = gsums[GAUSS_SUMGSQ] +
		        gkernel[i,j] ** 2
		    skip[i,j] = NO
		    npts = npts + 1.0
		} else {
		    #gkernel[i,j] = 0.0
		    ngkernel[i,j] = 0.0
		    dkernel[i,j] = 0.0
		    skip[i,j] = YES
		}
	    }
	}

	# Store the remaining sums.
	gsums[GAUSS_PIXELS] = npts
	gsums[GAUSS_DENOM] = gsums[GAUSS_SUMGSQ] - gsums[GAUSS_SUMG] ** 2 /
	    npts
	gsums[GAUSS_SGOP] = gsums[GAUSS_SUMG] / npts

	# Normalize the amplitude kernel.
	do j = 1, ny {
	     do i = 1, nx {
	        if (skip[i,j] == NO)
		    ngkernel[i,j] = (gkernel[i,j] - gsums[GAUSS_SGOP]) /
			gsums[GAUSS_DENOM]
	    }
	}

	# Normalize the sky kernel
	do j = 1, ny {
	    do i = 1, nx {
		if (skip[i,j] == NO)
		    dkernel[i,j] = dkernel[i,j] / npts
	    }
	}

	relerr = 1.0 / gsums[GAUSS_DENOM]

	return (sqrt (relerr))
end

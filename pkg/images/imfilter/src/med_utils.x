# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>

# MED_ELL_GAUSS -- Compute the parameters of the elliptical Gaussian.

procedure med_ell_gauss (sigma, ratio, theta, a, b, c, f, nx, ny)

real	sigma			#I sigma of Gaussian in x
real	ratio			#I ratio of half-width in y to x
real	theta			#I position angle of Gaussian
real	a, b, c, f		#O ellipse parameters
int	nx, ny			#O dimensions of the kernel

real	sx2, sy2, cost, sint, discrim
bool	fp_equalr ()

begin
	# Define some constants.
	sx2 = sigma ** 2
	sy2 = (ratio * sigma) ** 2
	cost = cos (DEGTORAD (theta))
	sint = sin (DEGTORAD (theta))

	# Compute the ellipse parameters.
	if (sigma <= 0.0) {
	    a = 0.0
	    b = 0.0
	    c = 0.0
	    f = 0.0
	    nx = 0
	    ny = 0
	} else if (fp_equalr (ratio, 0.0)) {

	    if (fp_equalr (theta, 0.0) || fp_equalr (theta, 180.)) {
		a = 1. / sx2
		b = 0.0
		c = 0.0
	    } else if (fp_equalr (theta, 90.0)) {
		a = 0.0
		b = 0.0
		c = 1. / sx2
	    } else
		call error (0, "MED_GAUSS_KERNEL: Cannot make 1D Gaussian.")

	    f = 0.5
	    nx = 2. * sigma * abs (cost) + 1.
	    ny = 2. * sigma * abs (sint) + 1.

	} else {

	    a = cost ** 2 / sx2 + sint ** 2 / sy2
	    b = 2. * (1.0 / sx2 - 1.0 / sy2) * cost * sint
	    c = sint ** 2 / sx2 + cost ** 2 / sy2
	    discrim = b ** 2 - 4. * a * c
	    f = 0.5
	    nx = 2. * sqrt (-8. * c * f / discrim) + 1.
	    ny = 2. * sqrt (-8. * a * f / discrim) + 1.
	}

	# Force the kernel to the next nearest odd integer.
	if (mod (nx, 2) == 0)
	    nx = nx + 1
	if (mod (ny, 2) == 0)
	    ny = ny + 1
end


# MED_RING_FILTER -- Construct the Gaussian kernel using the elliptical
# Gaussian parameters.

int procedure med_mkring (kernel, nx, ny, a1, b1, c1, f1, a2, b2, c2, f2)

short	kernel[nx,ny]		#O Gaussian kernel
int	nx, ny			#I dimensions of the kernel
real	a1, b1, c1, f1		#I inner ellipse parameters
real	a2, b2, c2, f2		#I outer ellipse parameters

int	i, j, x0, y0, x, y, nring
real	k1, k2

begin
	# Define some constants.
	x0 = nx / 2 + 1
	y0 = ny / 2 + 1

	# Compute the kernel.
	nring = 0
	do j = 1, ny {
	    y = j - y0
	    do i = 1, nx {
		x = i - x0
		k1 = 0.5 * (a1 * x ** 2 + c1 * y ** 2 + b1 * x * y)
		k2 = 0.5 * (a2 * x ** 2 + c2 * y ** 2 + b2 * x * y)
		if (k1 >= f1 && k2 <= f2) {
		    kernel[i,j] = 1
		    nring = nring + 1
		} else
		    kernel[i,j] = 0
	    }
	}

	return (nring)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>
include <math.h>

# T_GAUSS -- Convolve a list of IRAF images with a Gaussian

procedure t_gauss()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list

char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image

real	sigma					# Half width in x
real	ratio					# Ratio of halfwidth in y to x
real	theta					# Position angle
real	nsigma					# Extent of Gaussian

int	boundary				# Type of boundary extension
real	constant				# Constant boundary extension

char	str[SZ_LINE], imtemp[SZ_FNAME]
int	list1, list2, nxk, nyk, radsym
pointer	sp, im1, im2, kernel
real	a, b, c, f

bool	fp_equalr()
int	imtopen(), imtgetim(), imtlen(), clgwrd()
pointer	immap()
real	clgetr()

errchk	cnv_ell_gauss, cnv_gauss_kernel, cnv_convolve

begin
	# Get task parameters
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# Get kernel parameters
	sigma = clgetr ("sigma")
	ratio = clgetr ("ratio")
	theta = clgetr ("theta")
	nsigma = clgetr ("nsigma")

	# Get boundary extension parameters
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	if (boundary == BT_CONSTANT)
	    constant = clgetr ("constant")

	# Check list lengths
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Check parameters
	if (fp_equalr (sigma, 0.0))
	    call error (0, "T_GAUSS: Sigma must be greater than 0.")
	if (ratio < 0.0 || ratio > 1.0)
	    call error (0, "T_GAUSS: Ratio must be between 0 and 1.")
	if (theta < 0.0 || theta > 180.0)
	    call error (0, "T_GAUSS: Theta must be between 0 and 180 degrees.")
	if (nsigma <= 0.0)
	    call error (0, "T_GAUSS: Nsigma must be greater than 0.")
	if (fp_equalr (ratio, 0.0) && ! fp_equalr (theta, 0.0) && 
	    ! fp_equalr (theta, 90.0) && ! fp_equalr (theta, 180.0))
	    call error (0, "T_GAUSS: Cannot make 1D Gaussian at given theta.")

	# Convolve the images with a Gaussian
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    # Make temporary image
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    # Open images
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    kernel = NULL

	    # Convolve an image with a Gaussian
	    iferr {

		# calculate the ellipse parameters
		switch (IM_NDIM(im1)) {
		case 1:
		    call cnv_ell_gauss (sigma, 0.0, 0.0, nsigma, a, b,
		        c, f, nxk, nyk)
		    radsym = YES
		case 2:
		    call cnv_ell_gauss (sigma, ratio, theta, nsigma, a, b,
		        c, f, nxk, nyk)
		    if (ratio < 1.0)
			radsym = NO
		    else
			radsym = YES
		default:
		    call error (0,
		        "T_GAUSS: Cannot convolve 3D or greater image.")
		}

		# calculate the kernel
		call smark (sp)
		call salloc (kernel, nxk * nyk, TY_REAL)
		call cnv_gauss_kernel (Memr[kernel], nxk, nyk, a, b, c, f)

		# convolve image
		call cnv_convolve (im1, im2, Memr[kernel], nxk, nyk, boundary,
		    constant, radsym)

	    } then {
		call eprintf ("Error convolving image: %s\n")
		    call pargstr (image1)
		call erract (EA_WARN)
	        call imunmap (im1)
	        call imunmap (im2)
		call imdelete (image2)
	    } else {
	        call imunmap (im1)
	        call imunmap (im2)
	        call xt_delimtemp (image2, imtemp)
	    }

	    if (kernel != NULL)
	        call sfree (sp)
	    kernel = NULL
	}

	# close images
	call imtclose (list1)
	call imtclose (list2)
end

# define	HALFWIDTH	1.177410022

# CNV_ELL_GAUSS -- Make the elliptical Gaussian parameters

procedure cnv_ell_gauss (sigma, ratio, theta, nsigma, a, b, c, f, nx, ny)

real	sigma			# sigma of Gaussian in x
real	ratio			# Ratio of half-width in y to x
real	theta			# position angle of Gaussian
real	nsigma			# limit of convolution
real	a, b, c, f		# ellipse parameters
int	nx, ny			# dimensions of the kernel

real	sx2, sy2, cost, sint, discrim

bool	fp_equalr ()

begin
	sx2 = sigma ** 2
	sy2 = (ratio * sigma) ** 2

	cost = cos (DEGTORAD (theta))
	sint = sin (DEGTORAD (theta))

	# compute the ellipse parameters
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
		call error (0, "CNV_GAUSS_KERNEL: Cannot make 1D Gaussian.")

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

	# force to nearest odd integer
	if (mod (nx, 2) == 0)
	    nx = nx + 1
	if (mod (ny, 2) == 0)
	    ny = ny + 1
end

# CNV_GAUSS_KERNEL -- Make the Gaussian kernel using an elliptical Gaussian

procedure cnv_gauss_kernel (kernel, nx, ny, a, b, c, f)

real	kernel[nx,ny]		# Gaussian kernel
int	nx, ny			# dimensions of the kernel
real	a, b, c, f		# Ellipse parameters

int	i, j, x0, y0, x, y
real	norm

bool 	fp_equalr()

begin
	x0 = nx / 2 + 1
	y0 = ny / 2 + 1
	norm = 0.0

	# Compute the kernel
	do j = 1, ny {
	    y = j - y0
	    do i = 1, nx {
		x = i - x0
		kernel[i,j] = 0.5 * (a * x ** 2 + c * y ** 2 + b * x * y)
		if (kernel[i,j] <= f) {
		    kernel[i,j] = exp (-kernel[i,j])
		    norm = norm + kernel[i,j]
		} else
		    kernel[i,j] = 0.0
	    }
	}

	# Normalize the kernel
	if (! fp_equalr (norm, 0.0))
	    call adivkr (kernel, norm, kernel, nx * ny)
end

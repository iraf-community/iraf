# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <math.h>

define	GR_180		1		# Direction of maximum filter response 
define	GR_0		2		#
define	GR_90		3		#
define	GR_270		4		#
define	GR_135		5		#
define	GR_45		6		#
define	GR_225		7		#
define	GR_315		8		#

# T_GRADIENT -- Convolve a list of IRAF images with the specified gradient
# filter

procedure t_gradient()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list

char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image

int	filter					# Laplacian filter
int	boundary				# Type of boundary extension
real	constant				# Constant boundary extension

char	str[SZ_LINE], imtemp[SZ_FNAME]
int	list1, list2, nxk, nyk
pointer	sp, im1, im2, kernel

int	imtopen(), imtgetim(), imtlen(), clgwrd()
pointer	immap()
real	clgetr()

errchk	cnv_convolve

begin
	# Get task parameters.
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# Get boundary extension parameters.
	filter = clgwrd ("gradient", str, SZ_LINE,
	    ",180,0,90,270,135,45,225,315,")
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Check list lengths.
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0,
	        "The numbers of input and output images are not equal.")
	}

	# Convolve the images with gradient kernel.
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    # Make temporary image.
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    # Open images.
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    kernel = NULL

	    # Convolve an image with gradient kernel.
	    iferr {

		switch (IM_NDIM(im1)) {
		case 1:
		    nxk = 3
		    nyk = 1
		case 2:
		    nxk = 3
		    nyk = 3
		default:
		    call error (0,
		        "T_GRADIENT: The image has more than 2 dimensions.")
		}

		# Make the kernel.
		call smark (sp)
		call salloc (kernel, nxk * nyk, TY_REAL)
		call cnv_gradient_kernel (Memr[kernel], nxk, nyk, filter)

		# Gradient filter the image.
		call cnv_convolve (im1, im2, Memr[kernel], nxk, nyk, boundary,
		    constant, NO)

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

	# Close images.
	call imtclose (list1)
	call imtclose (list2)
end

# CNV_GRADIENT_KERNEL -- Make the gradient kernel.

procedure cnv_gradient_kernel (kernel, nx, ny, filter)

real	kernel[nx,ny]		# Gaussian kernel
int	nx, ny			# dimensions of the kernel
int	filter			# which filter

real	norm

begin
	if (ny == 1) {

	    switch (filter) {
	    case GR_0:
	        kernel[1,1] = -1. / 2.
	        kernel[2,1] = 0.
	        kernel[3,1] = 1. / 2.
	    case GR_180:
	        kernel[1,1] = 1. / 2.
	        kernel[2,1] = 0.
	        kernel[3,1] = -1. / 2.
	    default:
		call error (0,
		    "CNV_GRADIENT_KERNEL: Cannot compute 2D gradient")
	    }

	} else {

	    switch (filter) {
	    case GR_0:
		norm = 4. + 2. * SQRTOF2
		kernel[1,1] = -1. / norm
		kernel[2,1] = 0.
		kernel[3,1] = 1. / norm
		kernel[1,2] = - SQRTOF2 / norm
		kernel[2,2] = 0.
		kernel[3,2] = SQRTOF2 / norm
		kernel[1,3] = -1. / norm
		kernel[2,3] = 0.
		kernel[3,3] = 1. / norm
	    case GR_180:
		norm = 4. + 2. * SQRTOF2
		kernel[1,1] = 1. / norm
		kernel[2,1] = 0.
		kernel[3,1] = -1. / norm
		kernel[1,2] = SQRTOF2 / norm
		kernel[2,2] = 0.
		kernel[3,2] = - SQRTOF2 / norm
		kernel[1,3] = 1. / norm
		kernel[2,3] = 0.
		kernel[3,3] = -1. / norm
	    case GR_90:
		norm = 4. + 2. * SQRTOF2
		kernel[1,1] = -1. / norm
		kernel[2,1] = - SQRTOF2 / norm
		kernel[3,1] = -1. / norm
		kernel[1,2] = 0.
		kernel[2,2] = 0.
		kernel[3,2] = 0.
		kernel[1,3] = 1. / norm
		kernel[2,3] = SQRTOF2 / norm
		kernel[3,3] = 1. / norm
	    case GR_270:
		norm = 4. + 2. * SQRTOF2
		kernel[1,1] = 1. / norm
		kernel[2,1] = SQRTOF2 / norm
		kernel[3,1] = 1. / norm
		kernel[1,2] = 0.
		kernel[2,2] = 0.
		kernel[3,2] = 0.
		kernel[1,3] = -1. / norm
		kernel[2,3] = - SQRTOF2 / norm
		kernel[3,3] = -1. / norm
	    case GR_45:
		norm = 2. * SQRTOF2 + 1.
		kernel[1,1] = -1. / (2. * SQRTOF2 * norm)
		kernel[2,1] = -1. / norm
		kernel[3,1] = 0.
		kernel[1,2] = -1. / norm
		kernel[2,2] = 0.
		kernel[3,2] = 1. / norm
		kernel[1,3] = 0.
		kernel[2,3] = 1. / norm
		kernel[3,3] = 1. / (2. * SQRTOF2 * norm) 
	    case GR_225:
		norm = 2. * SQRTOF2 + 1.
		kernel[1,1] = 1. / (2. * SQRTOF2 * norm)
		kernel[2,1] = 1. / norm
		kernel[3,1] = 0.
		kernel[1,2] = 1. / norm
		kernel[2,2] = 0.
		kernel[3,2] = -1. / norm
		kernel[1,3] = 0.
		kernel[2,3] = -1. / norm
		kernel[3,3] = -1. / (2. * SQRTOF2 * norm) 
	    case GR_135:
		norm = 2. * SQRTOF2 + 1.
		kernel[1,1] = 0.
		kernel[2,1] = -1. / norm
		kernel[3,1] = -1. / (2. * SQRTOF2 * norm)
		kernel[1,2] = 1. / norm
		kernel[2,2] = 0.
		kernel[3,2] = -1. / norm
		kernel[1,3] = 1. / (2. * SQRTOF2 * norm) 
		kernel[2,3] = 1. / norm
		kernel[3,3] = 0.
	    case GR_315:
		norm = 2. * SQRTOF2 + 1.
		kernel[1,1] = 0.
		kernel[2,1] = 1. / norm
		kernel[3,1] = 1. / (2. * SQRTOF2 * norm)
		kernel[1,2] = -1. / norm
		kernel[2,2] = 0.
		kernel[3,2] = 1. / norm
		kernel[1,3] = -1. / (2. * SQRTOF2 * norm) 
		kernel[2,3] = -1. / norm
		kernel[3,3] = 0.
	    default:
		call error (0, "CNV_GRADIENT_KERNEL: Unknown filter.")
	    }
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <math.h>

define	LP_XYCENTER		1		# Horizontal and vertical
define	LP_DIAG			2		# Diagonal direction
define	LP_XYALL		3		# Average of 3 horizontal and
						# vertical
define	LP_XYDIAG		4		# Unsharp masking kernel

# T_LAPLACE -- Convolve a list of IRAF images with the Laplacian

procedure t_laplace()

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
	# Get the task parameters.
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# Get the boundary extension parameters.
	filter = clgwrd ("laplace", str, SZ_LINE,
	    ",xycentral,diagonals,xyall,xydiagonals,")
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Check the input and output image list lengths.
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Convolve the images with a Laplacian kernel.
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    # Make a temporary image.
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    # Open the input and output images.
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)
	    kernel = NULL

	    # Do the convolution.
	    iferr {

		switch (IM_NDIM(im1)) {
		case 1:
		    nxk = 3
		    nyk = 1
		case 2:
		    nxk = 3
		    nyk = 3
		default:
		    call error (0, "T_LAPLACE: Too many image dimensions.")
		}

		call smark (sp)
		call salloc (kernel, nxk * nyk, TY_REAL)
		call cnv_laplace_kernel (Memr[kernel], nxk, nyk, filter)

		call cnv_convolve (im1, im2, Memr[kernel], nxk, nyk, boundary,
		    constant, YES)

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

	# Close the list of images.
	call imtclose (list1)
	call imtclose (list2)
end


# CNV_LAPLACE_KERNEL -- Compute the Laplacian kernel.

procedure cnv_laplace_kernel (kernel, nx, ny, filter)

real	kernel[nx,ny]		# Gaussian kernel
int	nx, ny			# dimensions of the kernel
int	filter			# which filter

begin
	if (ny == 1) {

	    kernel[1,1] = -1.
	    kernel[2,1] = 2.
	    kernel[3,1] = -1.

	} else {

	    switch (filter) {
	    case LP_XYCENTER:
		kernel[1,1] = 0.
		kernel[2,1] = 1.
		kernel[3,1] = 0.
		kernel[1,2] = 1.
		kernel[2,2] = -4.
		kernel[3,2] = 1.
		kernel[1,3] = 0.
		kernel[2,3] = 1.
		kernel[3,3] = 0.
	    case LP_DIAG:
		kernel[1,1] = 1. / SQRTOF2
		kernel[2,1] = 0.
		kernel[3,1] = 1. / SQRTOF2
		kernel[1,2] = 0.
		kernel[2,2] = -4. / SQRTOF2
		kernel[3,2] = 0.
		kernel[1,3] = 1. / SQRTOF2
		kernel[2,3] = 0.
		kernel[3,3] = 1. / SQRTOF2
	    case LP_XYALL:
		kernel[1,1] = 2. / 3.
		kernel[2,1] = -1. / 3.
		kernel[3,1] = 2. / 3.
		kernel[1,2] = -1. / 3.
		kernel[2,2] = -4. / 3.
		kernel[3,2] = -1. / 3.
		kernel[1,3] = 2. / 3.
		kernel[2,3] = -1. / 3.
		kernel[3,3] = 2. / 3.
	    case LP_XYDIAG:
		kernel[1,1] = 1. / (SQRTOF2 * 2.)
		kernel[2,1] = .5
		kernel[3,1] = 1. / (SQRTOF2 * 2.)
		kernel[1,2] = .5
		kernel[2,2] = -2. - SQRTOF2
		kernel[3,2] = .5
		kernel[1,3] = 1. / (SQRTOF2 * 2.)
		kernel[2,3] = .5
		kernel[3,3] = 1. / (SQRTOF2 * 2.)
	    default:
		call error (0, "CNV_LAPLACE_KERNEL: Unknown filter.")
	    }
	}
end

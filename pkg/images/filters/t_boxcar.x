# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>

# T_BOXCAR -- Boxcar smooth a list of IRAF images

procedure t_boxcar()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list

char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image

int	boundary				# Type of boundary extension
real	constant				# Constant boundary extension

char	str[SZ_LINE], imtemp[SZ_FNAME]
int	list1, list2, kxdim, kydim
pointer	im1, im2

int	imtopen(), imtgetim(), imtlen(), clgeti(), clgwrd()
pointer	immap()
real	clgetr()

errchk	cnv_boxcar

begin
	# Get task parameters
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# Get filter parameters
	kxdim = clgeti ("xwindow")
	kydim = clgeti ("ywindow")

	# Get boundary extension parameters
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Check list lengths
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Boxcar smooth the images
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    # Make temporary image
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    # Open images
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Boxcar smooth an image
	    iferr {
		switch (IM_NDIM(im1)) {
		case 1:
		    kydim = 1
		case 2:
		    ;
		default:
		    call error (0, "T_CONVOLVE: Image dimension > 2.")
		}
		call cnv_boxcar (im1, im2, kxdim, kydim, boundary, constant)
	    } then {
		call eprintf ("Error smoothing image: %s\n")
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

	}

	# close images
	call imtclose (list1)
	call imtclose (list2)
end

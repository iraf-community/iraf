# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <error.h>

# T_FMEDIAN -- Median filter a list of images in x and y.

procedure t_fmedian()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list

char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image
char	imtemp[SZ_FNAME]			# Temporary file

int	boundary				# Type of boundary extension
real	constant				# Constant boundary extension

char	str[SZ_LINE]
int	list1, list2
pointer	im1, im2

bool	clgetb()
int	clgeti(), imtopen(), imtgetim(), imtlen(), clgwrd(), btoi()
pointer	immap()
real	clgetr()
errchk	fmd_box

include "fmedian.com"

begin
	# Get the task parameters.
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# The window sizes are converted to the nearest odd integer.
	xwindow = clgeti ("xwindow")
	if (mod (xwindow, 2) == 0)
	    xbox = xwindow + 1
	else
	    xbox = xwindow
	
	ywindow = clgeti ("ywindow")
	if (mod (ywindow, 2) == 0)
	    ybox = ywindow + 1
	else
	    ybox = ywindow

	# Get the quantization parameters.
	z1 = clgetr ("zmin")
	z2 = clgetr ("zmax")
	hmin = clgeti ("hmin")
	hmax = clgeti ("hmax")
	unmap = btoi (clgetb ("unmap"))

	# Get the boundary extension parameters.
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Open the input and output image lists.
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Median filter each set of input and output images.
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Find input image max and min if necessary.
	    if (IS_INDEF(z1) || IS_INDEF(z2)) 
	        call fmd_maxmin (im1, xbox, ybox, boundary, constant, zmin,
		    zmax)

	    # Median filter the image.
	    iferr {
		call fmd_box (im1, im2, boundary, constant)
	    } then {
		call eprintf ("Error median filtering image: %s\n")
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

	call imtclose (list1)
	call imtclose (list2)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>

# T_MODE -- Modal filter an image in x and y

procedure t_mode ()

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

int	clgeti(), imtopen(), imtgetim(), imtlen(), clgwrd()
pointer	immap()
real	clgetr()
errchk	mod_box

include "mode.com"

begin
	# Get task parameters
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)

	# Window sizes are rounded to the nearest odd integer
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

	# Check for the maximum filter size
	npts = xbox * ybox
	if (npts > MAX_FILTER)
	    call error (0, "T_MODE: Filter box too large")
	nhalf = (npts - 1) / 2
	nptsp1 = npts + 1

	# Get boundary extension parameters
	boundary = clgwrd ("boundary", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Mode filter the input images
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Modal filter an image
	    iferr {
		call mod_box (im1, im2, boundary, constant)
	    } then {
		call eprintf ("Error modal filtering image: %s\n")
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

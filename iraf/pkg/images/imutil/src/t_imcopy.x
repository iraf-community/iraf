# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMCOPY -- Copy image(s)
#
# The input images are given by an image template list.  The output
# is either a matching list of images or a directory.
# The number of input images may be either one or match the number of output
# images.  Image sections are allowed in the input images and are ignored
# in the output images.  If the input and output image names are the same
# then the copy is performed to a temporary file which then replaces the
# input image.

procedure t_imcopy()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
bool	verbose					# Print operations?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name

int	list1, list2, root_len

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb()

begin
	# Get input and output image template lists.

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    while (imtgetim (list1, image1, SZ_PATHNAME) != EOF) {

		# Strip the image section first because fnldir recognizes it
		# as part of a directory.  Place the input image name
		# without a directory or image section in string dirname1.

		call get_root (image1, image2, SZ_PATHNAME)
		root_len = fnldir (image2, dirname1, SZ_PATHNAME)
		call strcpy (image2[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)
		call img_imcopy (image1, image2, verbose)
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output image lists.

	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.

	    while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		(imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {

		call img_imcopy (image1, image2, verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end

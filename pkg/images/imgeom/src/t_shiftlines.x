# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>

# T_SHIFTLINES -- Shift image lines.
#
# The input and output images are given by image template lists.  The
# number of output images must match the number of input images.  Image
# sections are allowed in the input images and are ignored in the output
# images.  If the input and output image names are the same then the shift
# is performed to a temporary file which then replaces the input image.


procedure t_shiftlines()

char	imtlist1[SZ_LINE]		# Input image list
char	imtlist2[SZ_LINE]		# Output image list
real	shift				# Amount of pixel shift
int	boundary			# Type of boundary extension
real	constant			# Constant for boundary extension

char	image1[SZ_FNAME]		# Input image name
char	image2[SZ_FNAME]		# Output image name
char	imtemp[SZ_FNAME]		# Temporary file

char	str[SZ_LINE], interpstr[SZ_FNAME]
int	list1, list2, ishift
pointer	im1, im2, mw

bool	fp_equalr(), envgetb()
int	clgwrd(), imtopen(), imtgetim(), imtlen()
pointer	immap(), mw_openim()
real	clgetr()
errchk	sh_lines, sh_linesi, mw_openim, mw_shift, mw_saveim, mw_close

begin
	# Get input and output image template lists.
	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)

	# Get the shift, interpolation type, and boundary condition.
	shift = clgetr ("shift")
	call clgstr ("interp_type", interpstr, SZ_LINE)
	boundary = clgwrd ("boundary_type", str, SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	# Expand the input and output image lists.
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same")
	}

	ishift = shift

	# Do each set of input/output images.
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	    (imtgetim (list2, image2, SZ_FNAME) != EOF)) {

	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Shift the image.
	    iferr {

		if (fp_equalr (shift, real (ishift)))
		    call sh_linesi (im1, im2, ishift, boundary, constant)
		else
	            call sh_lines (im1, im2, shift, boundary, constant,
			interpstr)

		# Update the image WCS to reflect the shift.
		if (!envgetb ("nomwcs")) {
		    mw = mw_openim (im1)
		    call mw_shift (mw, shift, 1B)
		    call mw_saveim (mw, im2)
		    call mw_close (mw)
		}

	    } then {
		call eprintf ("Error shifting image: %s\n")
		    call pargstr (image1)
		call erract (EA_WARN)
	    }

	    # Unmap images.
	    call imunmap (im2)
	    call imunmap (im1)

	    # If in place operation replace the input image with the temporary
	    # image.
	    call xt_delimtemp (image2, imtemp)
	}

	call imtclose (list1)
	call imtclose (list2)
end

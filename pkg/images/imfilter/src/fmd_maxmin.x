# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include <imset.h>

# FMD_MAXMIN -- Find the maximum and minimum of an image allowing for
# boundary extension if necessary.

procedure fmd_maxmin (im, xbox, ybox, boundary, constant, minimum, maximum)

pointer	im		#I pointer to image
int	boundary	#I type of boundary extension
real	constant	#I constant for boundary extension
int	xbox, ybox	#I median filter size
real	minimum		#O image minimum
real	maximum		#O image maximum

int	i, col1, col2, line1, line2
pointer	buf
real	minval, maxval

pointer	imgs2r()

begin
	if (IM_LIMTIME(im) < IM_MTIME(im) || boundary == BT_PROJECT) {

	    # Set image boundary extension parameters.
	    call imseti (im, IM_TYBNDRY, boundary)
	    call imseti (im, IM_NBNDRYPIX, max (xbox / 2, ybox / 2))
	    call imsetr (im, IM_BNDRYPIXVAL, constant)

	    # Set the column and line boundaries.
	    col1 = 1 - xbox / 2
	    col2 = IM_LEN(im,1) + xbox / 2
	    line1 = 1 - ybox / 2
	    line2 = IM_LEN(im,2) + ybox / 2

	    # Initialize the max and min values.
	    minimum = MAX_REAL
	    maximum = -MAX_REAL

	    do i = line1, line2 {
		buf = imgs2r (im, col1, col2, i, i)
		call alimr (Memr[buf], col2 - col1 + 1, minval, maxval)
		minimum = min (minimum, minval)
		maximum = max (maximum, maxval)
	    }

	} else {

	    minimum = IM_MIN(im)
	    maximum = IM_MAX(im)

	    if (boundary == BT_CONSTANT) {
		if (constant < minimum)
		    minimum = constant
		if (constant > maximum)
		    maximum = constant
	    }
	}
end

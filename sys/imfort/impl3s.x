# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPL3S -- Put a line to a 3 dimensional image of type short.
# No automatic datatype conversion is performed.  It is illegal to reference
# out of bounds.

procedure impl3s (im, buf, lineno, bandno, ier)

pointer	im			# image descriptor
short	buf[ARB]		# user data buffer
int	lineno			# line number
int	bandno			# band number
int	ier

long	offset
int	nchars, npix
int	imwpix()

begin
	# Verify in bounds.
	if (lineno < 1 || lineno > IM_LEN(im,2)) {
	    ier = IE_YOOB
	    call im_seterrim (ier, im)
	    return
	} else if (bandno < 1 || bandno > IM_LEN(im,3)) {
	    ier = IE_ZOOB
	    call im_seterrim (ier, im)
	    return
	}

	# Verify the image is of type short.
	if (IM_PIXTYPE(im) != TY_SHORT) {
	    ier = IE_NOTSHORT
	    call im_seterrim (ier, im)
	    return
	}

	npix = IM_LEN(im,1)
	nchars = npix * SZ_SHORT

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im) +
	    ((bandno-1) * IM_LEN(im,2) + (lineno-1)) * IM_LINESIZE(im)

	# Write one line of data.
	if (nchars != imwpix (im, buf, nchars, offset, 0)) {
	    ier = IE_WRPIX
	    call im_seterrim (ier, im)
	    return
	}

	ier = OK
end

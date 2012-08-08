# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPL1S -- Put a line to a 1 dimensional image of type short.
# No automatic datatype conversion is performed.  It is illegal to reference
# out of bounds.

procedure impl1s (im, buf, ier)

pointer	im			# image descriptor
short	buf[ARB]		# user data buffer
int	ier

long	offset
int	nchars, npix
int	imwpix()

begin
	# Verify the image is of type short.
	if (IM_PIXTYPE(im) != TY_SHORT) {
	    ier = IE_NOTSHORT
	    call im_seterrim (ier, im)
	    return
	}

	npix = IM_LEN(im,1)
	nchars = npix * SZ_SHORT

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im)

	# Write one line of data.
	if (nchars != imwpix (im, buf, nchars, offset, 0)) {
	    ier = IE_WRPIX
	    call im_seterrim (ier, im)
	    return
	}

	ier = OK
end

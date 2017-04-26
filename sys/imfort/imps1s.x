# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPS1S -- Put a section to a 1 dimensional image of type short.
# No automatic datatype conversion is performed.  It is illegal to reference
# out of bounds.

procedure imps1s (im, buf, i1, i2, ier)

pointer	im			# image descriptor
short	buf[ARB]		# user data buffer
int	i1, i2			# first, last columns
int	ier

long	offset
int	nchars, npix
int	imwpix()

begin
	# Verify in bounds.
	if (i1 < 1 || i2 > IM_LEN(im,1) || i1 > i2) {
	    ier = IE_XOOB
	    call im_seterrim (ier, im)
	    return
	} else if (IM_PIXTYPE(im) != TY_SHORT) {
	    ier = IE_NOTSHORT
	    call im_seterrim (ier, im)
	    return
	}

	npix = i2 - i1 + 1
	nchars = npix * SZ_SHORT

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im) + (i1-1) * SZ_SHORT

	# Write one line of data.
	if (nchars != imwpix (im, buf, nchars, offset, 0)) {
	    ier = IE_WRPIX
	    call im_seterrim (ier, im)
	    return
	}

	ier = OK
end

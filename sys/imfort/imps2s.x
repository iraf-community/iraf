# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPS2S -- Put a section to a 2 dimensional image of type short.
# No automatic datatype conversion is performed.  It is illegal to reference
# out of bounds.

procedure imps2s (im, buf, i1, i2, j1, j2, ier)

pointer	im			# image descriptor
short	buf[ARB]		# user data buffer
int	i1, i2			# first, last columns
int	j1, j2			# first, last lines
int	ier

long	offset
int	nchars, npix, ip, j
int	imwpix()

begin
	# Verify in bounds.
	if (i1 < 1 || i2 > IM_LEN(im,1) || i1 > i2) {
	    ier = IE_XOOB
	    call im_seterrim (ier, im)
	    return
	} else if (j1 < 1 || j2 > IM_LEN(im,2) || j1 > j2) {
	    ier = IE_YOOB
	    call im_seterrim (ier, im)
	    return
	} else if (IM_PIXTYPE(im) != TY_SHORT) {
	    ier = IE_NOTSHORT
	    call im_seterrim (ier, im)
	    return
	}

	npix = i2 - i1 + 1
	nchars = npix * SZ_SHORT
	ip = 1

	do j = j1, j2 {
	    # Compute offset into pixel file.
	    offset = IM_PIXOFF(im) +
		((j-1) * IM_LINESIZE(im) + (i1-1)) * SZ_SHORT

	    # Write one line of data.
	    if (nchars != imwpix (im, buf[ip], nchars, offset, 0)) {
		ier = IE_WRPIX
		call im_seterrim (ier, im)
		return
	    }

	    ip = ip + npix
	}

	ier = OK
end

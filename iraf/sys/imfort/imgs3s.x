# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGS3S -- Get a section from 3 dimensional image of type short.
# No automatic datatype conversion is performed.  It is illegal to reference
# out of bounds.

procedure imgs3s (im, buf, i1, i2, j1, j2, k1, k2, ier)

pointer	im			# image descriptor
short	buf[ARB]		# user data buffer
int	i1, i2			# first, last columns
int	j1, j2			# first, last lines
int	k1, k2			# first, last bands
int	ier

long	offset
int	nchars, npix, op, j, k
int	bfread()

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
	} else if (k1 < 1 || k2 > IM_LEN(im,3) || k1 > k2) {
	    ier = IE_ZOOB
	    call im_seterrim (ier, im)
	    return
	} else if (IM_PIXTYPE(im) != TY_SHORT) {
	    ier = IE_NOTSHORT
	    call im_seterrim (ier, im)
	    return
	}

	npix = i2 - i1 + 1
	nchars = npix * SZ_SHORT
	op = 1

	do k = k1, k2 {
	    do j = j1, j2 {
		# Compute offset into pixel file.
		offset = IM_PIXOFF(im) + (i1-1) * SZ_SHORT +
		    ((k-1) * IM_LEN(im,2) + (j-1)) * IM_LINESIZE(im)

		# Read one line of data.
		if (nchars != bfread (IM_PIXFP(im), buf[op], nchars, offset)) {
		    ier = IE_RDPIX
		    call im_seterrim (ier, im)
		    return
		}

		# Swap bytes if necessary.
		call imswap (im, buf[op], nchars)

		op = op + npix
	    }
	}

	ier = OK
end

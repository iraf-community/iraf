# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGS1R -- Get a section from an image of type short or real.  Automatic
# datatype conversion from short to real is performed if necessary.  It is
# illegal to reference out of bounds.

procedure imgs1r (im, buf, i1, i2, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	i1, i2			# first, last column
int	ier

long	offset
int	nchars, npix
int	bfread()

begin
	# Verify in bounds.
	if (i1 < 1 || i2 > IM_LEN(im,1) || i1 > i2) {
	    ier = IE_XOOB
	    call im_seterrim (ier, im)
	    return
	} else if (IM_PIXTYPE(im) != TY_SHORT && IM_PIXTYPE(im) != TY_REAL) {
	    ier = IE_PIXTYPE
	    call im_seterrim (ier, im)
	    return
	}

	npix = i2 - i1 + 1
	nchars = npix * IM_SZPIXEL(im)

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im) + (i1-1) * IM_SZPIXEL(im)

	# Read data.
	if (nchars != bfread (IM_PIXFP(im), buf, nchars, offset)) {
	    ier = IE_RDPIX
	    call im_seterrim (ier, im)
	    return
	}

	# Swap bytes if necessary.
	call imswap (im, buf, nchars)

	# Convert the datatype if necessary.
	if (IM_PIXTYPE(im) == TY_SHORT)
	    call achtsr (buf, buf, npix)

	ier = OK
end

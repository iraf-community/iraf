# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGL1R -- Get a line from an image of type short or real.  Automatic
# datatype conversion from short to real is performed if necessary.
# It is illegal to reference out of bounds.

procedure imgl1r (im, buf, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	ier

long	offset
int	nchars, npix
int	bfread()

begin
	npix = IM_LEN(im,1)
	nchars = npix * IM_SZPIXEL(im)

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im)

	# Read one line of data.
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

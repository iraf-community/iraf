# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGL2R -- Get a line from an image of type short or real.  Automatic
# datatype conversion from short to real is performed if necessary.
# It is illegal to reference out of bounds.

procedure imgl2r (im, buf, lineno, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	lineno			# line number
int	ier

long	offset
int	nchars, npix
int	bfread()

begin
	# Verify in bounds.
	if (lineno < 1 || lineno > IM_LEN(im,2)) {
	    ier = IE_YOOB
	    call im_seterrim (ier, im)
	    return
	}

	npix = IM_LEN(im,1)
	nchars = npix * IM_SZPIXEL(im)

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im) + (lineno-1) * IM_LINESIZE(im)

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

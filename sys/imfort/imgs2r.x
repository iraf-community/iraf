# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGS2R -- Get a section from an image of type short or real.  Automatic
# datatype conversion from short to real is performed if necessary.  It is
# illegal to reference out of bounds.

procedure imgs2r (im, buf, i1, i2, j1, j2, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	i1, i2			# first, last column
int	j1, j2			# line number
int	ier

long	offset
int	nchars, npix, j, op
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
	} else if (IM_PIXTYPE(im) != TY_SHORT && IM_PIXTYPE(im) != TY_REAL) {
	    ier = IE_PIXTYPE
	    call im_seterrim (ier, im)
	    return
	}

	npix = i2 - i1 + 1
	nchars = npix * IM_SZPIXEL(im)
	op = 1

	do j = j1, j2 {
	    # Compute offset into pixel file.
	    offset = IM_PIXOFF(im) + (j-1) * IM_LINESIZE(im) +
		(i1-1) * IM_SZPIXEL(im)

	    # Read one line of data.
	    if (nchars != bfread (IM_PIXFP(im), buf[op], nchars, offset)) {
		ier = IE_RDPIX
		call im_seterrim (ier, im)
		return
	    }

	    # Swap bytes if necessary.
	    call imswap (im, buf[op], nchars)

	    # Convert the datatype if necessary.
	    if (IM_PIXTYPE(im) == TY_SHORT)
		call achtsr (buf[op], buf[op], npix)

	    op = op + npix
	}

	ier = OK
end

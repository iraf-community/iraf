# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPS1R -- Put a section to an image of type short or real.  Automatic
# datatype conversion from real to short is performed if necessary.
# It is illegal to reference out of bounds.

procedure imps1r (im, buf, i1, i2, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	i1, i2			# first, last column
int	ier

pointer	bp
long	offset
int	nchars, npix
int	imwpix()
errchk	malloc

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

	# Need an extra line buffer for the type conversion in this case.
	if (IM_PIXTYPE(im) == TY_SHORT) {
	    bp = IM_LINEBUFP(im)
	    if (bp == NULL) {
		call malloc (bp, IM_LEN(im,1), TY_SHORT)
		IM_LINEBUFP(im) = bp
	    }
	}

	npix = (i2 - i1 + 1)
	nchars = npix * IM_SZPIXEL(im)

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im) + (i1-1) * IM_SZPIXEL(im)

	if (IM_PIXTYPE(im) == TY_SHORT) {
	    # Convert the pixels from real to short before writing to the
	    # pixel file.

	    call achtrs (buf, Mems[bp], npix)

	    # Write one line of data.
	    if (nchars != imwpix (im, Mems[bp], nchars, offset, 1)) {
		ier = IE_WRPIX
		call im_seterrim (ier, im)
		return
	    }

	} else {
	    # Write one line of data.
	    if (nchars != imwpix (im, buf, nchars, offset, 0)) {
		ier = IE_WRPIX
		call im_seterrim (ier, im)
		return
	    }
	}

	ier = OK
end

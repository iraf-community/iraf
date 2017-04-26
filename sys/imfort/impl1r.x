# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPL1R -- Put a line to an image of type short or real.  Automatic
# datatype conversion from real to short is performed if necessary.
# It is illegal to reference out of bounds.

procedure impl1r (im, buf, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	ier

pointer	bp
long	offset
int	nchars, npix
int	imwpix()
errchk	malloc

begin
	# Need an extra line buffer for the type conversion in this case.
	if (IM_PIXTYPE(im) == TY_SHORT) {
	    bp = IM_LINEBUFP(im)
	    if (bp == NULL) {
		call malloc (bp, IM_LEN(im,1), TY_SHORT)
		IM_LINEBUFP(im) = bp
	    }
	}

	npix = IM_LEN(im,1)
	nchars = npix * IM_SZPIXEL(im)

	# Compute offset into pixel file.
	offset = IM_PIXOFF(im)

	if (IM_PIXTYPE(im) == TY_SHORT) {
	    # Convert the pixels before writing to the pixel file.
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

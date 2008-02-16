# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMPS3R -- Put a section to an image of type short or real.  Automatic
# datatype conversion from real to short is performed if necessary.
# It is illegal to reference out of bounds.

procedure imps3r (im, buf, i1, i2, j1, j2, k1, k2, ier)

pointer	im			# image descriptor
real	buf[ARB]		# user data buffer
int	i1, i2			# first, last column
int	j1, j2			# line numbers
int	k1, k2			# band numbers
int	ier

pointer	bp
long	offset
int	nchars, npix, ip, j, k
int	imwpix()
errchk	malloc

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
	ip = 1

	do k = k1, k2 {
	    do j = j1, j2 {
		# Compute offset into pixel file.
		offset = IM_PIXOFF(im) + (i1-1) * IM_SZPIXEL(im) +
		    ((k-1) * IM_LEN(im,2) + (j-1)) * IM_LINESIZE(im)

		if (IM_PIXTYPE(im) == TY_SHORT) {
		    # Convert the pixels from real to short before writing to
		    # the pixel file.

		    call achtrs (buf[ip], Mems[bp], npix)

		    # Write one line of data.
		    if (nchars != imwpix (im, Mems[bp], nchars, offset, 1)) {
			ier = IE_WRPIX
			call im_seterrim (ier, im)
			return
		    }

		} else {
		    # Write one line of data.
		    if (nchars != imwpix (im, buf[ip], nchars, offset, 0)) {
			ier = IE_WRPIX
			call im_seterrim (ier, im)
			return
		    }
		}

		ip = ip + npix
	    }
	}

	ier = OK
end

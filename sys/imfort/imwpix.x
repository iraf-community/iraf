# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	"imfort.h"

# IMWPIX -- Write a block of pixels to an image.  This is equivalent to
# a binary file write to the pixel file (bfwrit) except that the pixels
# are swapped if necessary.

int procedure imwpix (im, buf, nchars, offset, inplace)

pointer	im			#I image descriptor
char	buf[ARB]		#I pixel data
int	nchars			#I nchars of data to be written
int	offset			#I file offset in pixel file
int	inplace			#I nonzero if ok to modify input data buffer

pointer	sp, bp
int	nbytes, status
int	bfwrit()

begin
	# Just write out the data if no swapping is required.
	if (IM_SWAP(im) == NO)
	    return (bfwrit (IM_PIXFP(im), buf, nchars, offset))

	# Swap, but use the input buffer directly.
	if (inplace != 0) {
	    call imswap (im, buf, nchars)
	    return (bfwrit (IM_PIXFP(im), buf, nchars, offset))
	}

	# We need to swap into a private buffer.
	call smark (sp)
	call salloc (bp, nchars, TY_CHAR)

	# Swap into the output buffer.
	nbytes = nchars * SZB_CHAR
	switch (IM_SZPIXEL(im) * SZB_CHAR) {
	case 2:
	    call bswap2 (buf, 1, Memc[bp], 1, nbytes)
	case 4:
	    call bswap4 (buf, 1, Memc[bp], 1, nbytes)
	case 8:
	    call bswap8 (buf, 1, Memc[bp], 1, nbytes)
	}

	status = bfwrit (IM_PIXFP(im), Memc[bp], nchars, offset)

	call sfree (sp)
	return (status)
end

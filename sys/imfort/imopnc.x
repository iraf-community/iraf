# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMOPNC -- Open a new copy of an existing image, with the same dimensions,
# size, pixel type, and non-pixel header fields as the original, but without
# copying any of the pixel data.  The new image is left open for read-write
# access and a descriptor for the new image is returned as an argument.

procedure imopnc (nimage, o_im, n_im, ier)

%	character*(*) nimage
pointer	o_im, n_im		# old, new image descriptors
int	ier

int	naxis, pixtype, junk, i
int	axlen[IM_MAXDIM]
define	quit_ 91

begin
	n_im = NULL

	# Get the physical parameters of the old image.
	pixtype = IM_PIXTYPE(o_im)
	naxis = IM_NDIM(o_im)
	do i = 1, naxis
	    axlen[i] = IM_LEN(o_im,i)

	# Create and open the new image.
	call imcrea (nimage, axlen, naxis, pixtype, ier)
	if (ier != OK)
	    goto quit_
	call imopen (nimage, RW, n_im, ier)
	if (ier != OK)
	    goto quit_

	# Pass the header of the old image to the new.
	call imhcpy (o_im, n_im, ier)
	if (ier != OK)
	    goto quit_

	return

quit_
	# Error recovery.
	if (n_im != NULL)
	    call imclos (n_im, junk)
end

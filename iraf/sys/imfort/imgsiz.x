# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMGSIZ -- Get the physical attributes (size and type) of an image.

procedure imgsiz (im, axlen, naxis, pixtype, ier)

pointer	im			# image descriptor
int	axlen[IM_MAXDIM]	# receives axis lengths
int	naxis			# receives number of axes
int	pixtype			# receives pixel type
int	ier			# receives error status

size_t	sz_val
bool	strne()

begin
	if (strne (IM_MAGIC(im), "imhdr"))
	    ier = IE_MAGIC
	else {
	    sz_val = IM_MAXDIM
	    call amovl (IM_LEN(im,1), axlen, sz_val)
	    naxis = IM_NDIM(im)
	    pixtype = IM_PIXTYPE(im)
	    ier = OK
	}
end

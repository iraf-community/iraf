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

bool	strne()

begin
	if (strne (IM_MAGIC(im), "imhdr"))
	    ier = IE_MAGIC
	else {
	    call amovl (IM_LEN(im,1), axlen, IM_MAXDIM)
	    naxis = IM_NDIM(im)
	    pixtype = IM_PIXTYPE(im)
	    ier = OK
	}
end

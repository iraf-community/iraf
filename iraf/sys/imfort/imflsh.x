# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMFLSH -- Flush any buffered image data, i.e., synchronize the in-core
# version of an image with that on disk.

procedure imflsh (im, ier)

pointer	im			# image descriptor
int	ier

int	status
int	bfflsh()

begin
	ier = OK

	# Flush any buffered output pixel data.
	status = bfflsh (IM_PIXFP(im))
	if (status == ERR)
	    ier = IE_FLUSH

	# Update the image header if it has been modified.
	if (IM_HDRFP(im) != NULL) {
	    if (IM_UPDATE(im) == YES) {
		call imf_updhdr (im, status)
		if (status == ERR && ier == OK)
		    ier = IE_UPDHDR
	    }
	} else if (IM_UPDATE(im) == YES && ier == OK)
	    ier = IE_UPDRO
end

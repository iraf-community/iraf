# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"
include	"oif.h"

# IMF_UPDHDR -- Update the image header.

procedure imf_updhdr (im, status)

pointer	im			# image descriptor
int	status			# return status

pointer	fp
int	imwrhdr()

begin
	fp = IM_HDRFP(im)
	if (imwrhdr (fp, im, TY_IMHDR) != ERR)
	    IM_UPDATE(im) = NO
end

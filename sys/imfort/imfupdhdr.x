# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMF_UPDHDR -- Update the image header.

procedure imf_updhdr (im, status)

pointer	im			# image descriptor
int	status			# return status

long	offset
int	len_userarea, nchars, fp
int	strlen(), bfwrit()

begin
	fp = IM_HDRFP(im)

	# Determine the actual length of the image header.  While an image
	# is open the user area is typically very large to allow for the
	# addition of new parameters.  On disk we only want to use as much
	# space as is actually needed to save the header.

	len_userarea = strlen (Memc[IM_USERAREA(im)])
	IM_HDRLEN(im) = LEN_IMHDR +
	    (len_userarea+1 + SZ_STRUCT-1) / SZ_STRUCT

	# Rewrite the image header.
	nchars = IM_HDRLEN(im) * SZ_STRUCT
	offset = 1

	if (bfwrit (fp, IM_MAGIC(im), nchars, offset) < nchars)
	    status = ERR
	else
	    status = OK

	if (status == OK)
	    IM_UPDATE(im) = NO
end

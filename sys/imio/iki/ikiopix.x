# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	"iki.h"

# IKI_OPIX -- Open or create the pixel storage file, if any.  We are called by
# IMIO when i/o is first done to the image.  In the case of a new image, IMIO
# will already have set up the IM_NDIM and IM_LEN fields of the image header.

procedure iki_opix (im)

pointer	im			#I image descriptor
int	status
include	"iki.com"

begin
	iferr (call zcall2 (IKI_OPIX(IM_KERNEL(im)), im, status))
	    status = ERR
	if (status == ERR)
	    call syserrs (SYS_IKIOPIX, IM_NAME(im))
end

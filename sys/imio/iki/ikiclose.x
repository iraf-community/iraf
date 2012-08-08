# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <imhdr.h>
include <imio.h>
include "iki.h"

# IKI_CLOSE -- Physically close an image opened under the IKI.  It is not
# necessary to update the image header or flush any pixel data, as IMIO will
# already have performed those functions.

procedure iki_close (im)

pointer	im			#I image descriptor

int	status
include	"iki.com"

begin
	iferr (call zcall2 (IKI_CLOSE(IM_KERNEL(im)), im, status))
	    status = ERR
	if (status == ERR)
	    call syserrs (SYS_IKICLOSE, IM_NAME(im))
end

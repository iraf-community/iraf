# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imio.h>
include <plset.h>

# PLF_CLOSE -- Close a mask image.

procedure plf_close (im, status)

pointer	im			#I image descriptor
int	status			#O output status

begin
	if (IM_PFD(im) != NULL)
	    call close (IM_PFD(im))
	if (and (IM_PLFLAGS(im), PL_CLOSEPL) != 0)
	    call pl_close (IM_PL(im))

	IM_PL(im) = NULL
end

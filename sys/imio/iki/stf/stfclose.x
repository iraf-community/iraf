# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imio.h>
include	"stf.h"

# STF_CLOSE -- Close an STF format image.  There is little for us to do, since
# IMIO will already have updated the header if necessary and flushed any pixel
# output.  Neither do we have to deallocate the IMIO descriptor, since it was
# allocated by IMIO.

procedure stf_close (im, status)

pointer	im			# image descriptor
int	status

pointer	stf
errchk	close

begin
	stf = IM_KDES(im)

	# Close the pixel file and header file, if open.
	if (STF_PFD(stf) != NULL)
	    call close (STF_PFD(stf))
	if (IM_HFD(im) != NULL)
	    call close (IM_HFD(im))

	# Deallocate the STF descirptor.
	call mfree (IM_KDES(im), TY_STRUCT)
	status = OK
end

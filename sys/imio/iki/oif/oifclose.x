# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <protect.h>
include <error.h>
include <imhdr.h>
include <imio.h>

# OIF_CLOSE -- Close an OIF format image.  There is little for us to do, since
# IMIO will already have updated the header if necessary and flushed any pixel
# output.  Neither do we have to deallocate the IMIO descriptor, since it was
# allocated by IMIO.

procedure oif_close (im, status)

pointer	im			# image descriptor
int	status

int	junk
int	protect()

begin
	# Close the pixel file and header file, if open.
	if (IM_PFD(im) != NULL)
	    call close (IM_PFD(im))
	if (IM_HFD(im) != NULL)
	    call close (IM_HFD(im))

	# If we are closing a new image, set delete protection on the
	# header file to prevent the user from using DELETE to delete
	# the image header file, which would leave a headerless pixel
	# storage file lying about somewhere.

	if (IM_ACMODE(im) == NEW_IMAGE || IM_ACMODE(im) == NEW_COPY)
	    iferr (junk = protect (IM_HDRFILE(im), SET_PROTECTION))
		call erract (EA_WARN)
end

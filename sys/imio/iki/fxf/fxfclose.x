# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <imhdr.h>
include <imio.h>
include	"fxf.h"

# FXF_CLOSE -- Close a FITS format image.  There is little for us to do, since
# IMIO will already have updated the header if necessary and flushed any pixel
# output.  Neither do we have to deallocate the IMIO descriptor, since it was
# allocated by IMIO.

procedure fxf_close (im, status)

pointer	im		#I image descriptor
int	status		#O status value

pointer fit
errchk	close

begin
	fit = IM_KDES(im)

	# Reset the IEEE interface to its original state.
	switch (IM_ACMODE(im)) {
	case READ_ONLY, READ_WRITE, WRITE_ONLY:
	    call ieesnanr (FIT_SVNANR(fit))
	    call ieesmapr (FIT_SVMAPRIN(fit), FIT_SVMAPROUT(fit))
	    call ieesnand (FIT_SVNAND(fit))
	    call ieesmapd (FIT_SVMAPDIN(fit), FIT_SVMAPDOUT(fit))
	default:
	    ;
	}

	# Close the fits file.
	if (IM_PFD(im) != NULL) 
	    call close (IM_PFD(im))
	
	# Deallocate the FIT descriptor.
	call mfree (fit, TY_STRUCT)

	status = OK
end

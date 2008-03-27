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

size_t	sz_val
int	i, i_max
pointer fit
errchk	close

include "fxf.com"

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

	#
	do i = 0, num_fit-1 {
	    if ( Memp[fit_ptrs0+i] == fit ) Memp[fit_ptrs0+i] = NULL
	}
	# Setup fit address table
	i_max = -1
	do i = 0, num_fit-1 {
	    if ( Memp[fit_ptrs0+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (fit_ptrs0, TY_POINTER)
	    fit_ptrs0 = NULL
	} else {
	    call realloc (fit_ptrs0, sz_val, TY_POINTER)
	}
	fit_ptrs = fit_ptrs0 - 1
	num_fit = sz_val
	#
	
	# Deallocate the FIT descriptor.
	call mfree (fit, TY_STRUCT)

	status = OK
end

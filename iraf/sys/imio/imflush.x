# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMFLUSH -- Flush the output buffer.  The output buffer may contain
# pixels of any datatype.  The entry point of the datatype specific
# flush procedure is saved in the image descriptor by IMPGS?.

procedure imflush (imdes)

pointer	imdes

begin
	if (IM_PFD(imdes) != NULL && IM_FLUSH(imdes) == YES) {
	    call zcall1 (IM_FLUSHEPA(imdes), imdes)
	    call flush (IM_PFD(imdes))
	}
end

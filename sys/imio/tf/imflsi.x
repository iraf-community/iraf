# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMFLS? -- Flush the output buffer, if necessary.  Convert the datatype
# of the pixels upon output, if the datatype of the pixels in the imagefile
# is different than that requested by the calling program.

procedure imflsi (imdes)

pointer	imdes
pointer	bdes, bp
errchk	imflsh

begin
	# Ignore the flush request if the output buffer has already been
	# flushed.

	if (IM_FLUSH(imdes) == YES) {
	    bdes = IM_OBDES(imdes)
	    bp = BD_BUFPTR(bdes)

	    # Convert datatype of pixels, if necessary, and flush buffer.
	    if (IM_PIXTYPE(imdes) != TY_INT || SZ_INT != SZ_INT32) {
		call impaki (Memc[bp], Memc[bp], BD_NPIX(bdes),
		    IM_PIXTYPE(imdes))
	    }

	    call imflsh (imdes, bp, BD_VS(bdes,1), BD_VE(bdes,1), BD_NDIM(bdes))

	    IM_FLUSH(imdes) = NO
	}
end

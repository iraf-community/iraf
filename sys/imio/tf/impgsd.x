# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMPGS? -- Put a general section of a specific datatype.

pointer procedure impgsd (imdes, vs, ve, ndim)

pointer	imdes
long	vs[IM_MAXDIM], ve[IM_MAXDIM]
pointer	bp, imgobf()
int	ndim
extern	imflsd()
errchk	imflush, imgobf

begin
	# Flush the output buffer, if appropriate.  IMFLUSH calls
	# one of the IMFLS? routines, which write out the section.

	if (IM_FLUSH(imdes) == YES)
	    call zcall1 (IM_FLUSHEPA(imdes), imdes)

	# Get an (output) buffer to put the pixels into.  Save the
	# section parameters in the image descriptor.  Save the epa
	# of the typed flush procedure in the image descriptor.

	bp = imgobf (imdes, vs, ve, ndim, TY_DOUBLE)
	call zlocpr (imflsd, IM_FLUSHEPA(imdes))
	IM_FLUSH(imdes) = YES

	return (bp)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMCLOS -- Close an IMFORT image.  If the image was opened read only the
# header file will already have been closed, otherwise it must be updated
# if it has been modified.

procedure imclos (im, ier)

pointer	im			# image descriptor
int	ier			# receives error status

int	status

begin
	call imflsh (im, ier)

	# Close the pixel file.
	if (IM_PIXFP(im) != NULL) {
	    call bfclos (IM_PIXFP(im), status)
	    if (status == ERR && ier == OK)
		ier = IE_CLSPIX
	}

	# Close the header file.
	if (IM_HDRFP(im) != NULL) {
	    call bfclos (IM_HDRFP(im), status)
	    if (status == ERR && ier == OK)
		ier = IE_CLSHDR
	}

	if (IM_LINEBUFP(im) != NULL)
	    call mfree (IM_LINEBUFP(im), TY_SHORT)
	call mfree (im, TY_STRUCT)
end

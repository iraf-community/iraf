# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMSTATR -- Get the real value of an IMIO parameter.

real procedure imstatr (im, param)

pointer	im			#I image descriptor
int	param			#I parameter to be set

int	value
int	imstati()
errchk	imstati

begin
	switch (param) {
	case IM_BNDRYPIXVAL:
	    return (IM_OOBPIX(im))
	default:
	    value = imstati (im, param)
	    if (IS_INDEFI (value))
		return (INDEFR)
	    else
		return (value)
	}
end

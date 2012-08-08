# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# MW_OPENIM -- Open an MWCS descriptor on an image, loading the descriptor
# from the image if there is one.  The MWCS descriptor is allocated after
# the WCS cards are read in mw_loadim so that the WCS dimensionality can
# be determined when the image header is dataless.

pointer procedure mw_openim (im)

pointer	im			#I pointer to image descriptor

pointer	mw

begin
	mw = NULL
	call mw_loadim (mw, im)
	return (mw)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# MW_OPENIM -- Open an MWCS descriptor on an image, loading the descriptor
# from the image if there is one.

pointer procedure mw_openim (im)

pointer	im			#I pointer to image descriptor

pointer	mw
pointer	mw_open()
errchk	mw_open

begin
	mw = mw_open (NULL, IM_NPHYSDIM(im))
	call mw_loadim (mw, im)
	return (mw)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<plset.h>

# PLF_UPDHDR -- Update the image header.

procedure plf_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O output status

pointer	bp
int	nchars, flags, sz_buf
int	im_pmsvhdr()

begin
	status = OK

	flags = 0
	if (IM_ACMODE(im) == READ_WRITE)
	    flags = PL_UPDATE

	bp = NULL
	iferr {
	    nchars = im_pmsvhdr (im, bp, sz_buf)
	    call pl_savef (IM_PL(im), IM_HDRFILE(im), Memc[bp], flags)
	} then
	    status = ERR

	call mfree (bp, TY_CHAR)
end

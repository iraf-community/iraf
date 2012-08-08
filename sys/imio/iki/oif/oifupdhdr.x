# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	"oif.h"

# OIF_UPDHDR -- Update the image header.

procedure oif_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O return status

int	hfd
errchk	imerr, open, oif_wrhdr, flush
int	open()

begin
	status = OK
	hfd = IM_HFD(im)

	if (IM_ACMODE(im) == READ_ONLY)
	    call imerr (IM_NAME(im), SYS_IMUPIMHDR)
	if (hfd == NULL)
	    hfd = open (IM_HDRFILE(im), READ_WRITE, BINARY_FILE)

	call oif_wrhdr (hfd, im, TY_IMHDR)
	call flush (hfd)

	if (IM_HFD(im) == NULL)
	    call close (hfd)
end

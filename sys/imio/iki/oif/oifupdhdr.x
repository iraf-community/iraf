# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>

# OIF_UPDHDR -- Update the image header.

procedure oif_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O return status

int	len_userarea, hfd
int	strlen(), open()
errchk	imerr, open

begin
	status = OK
	hfd = IM_HFD(im)

	if (IM_ACMODE(im) == READ_ONLY)
	    call imerr (IM_NAME(im), SYS_IMUPIMHDR)
	if (hfd == NULL)
	   hfd = open (IM_HDRFILE(im), READ_WRITE, BINARY_FILE)

	# Determine the actual length of the image header.  While an image
	# is open the user area is typically very large to allow for the
	# addition of new parameters.  On disk we only want to use as much
	# space as is actually needed to save the header.

	len_userarea = strlen (Memc[IM_USERAREA(im)])
	IM_HDRLEN(im) = LEN_IMHDR +
	    (len_userarea+1 + SZ_STRUCT-1) / SZ_STRUCT

	# Rewrite the entire image header.
	call seek  (hfd, BOFL)
	call write (hfd, IM_MAGIC(im), IM_HDRLEN(im) * SZ_STRUCT)
	call flush (hfd)

	if (IM_HFD(im) == NULL)
	    call close (hfd)
end

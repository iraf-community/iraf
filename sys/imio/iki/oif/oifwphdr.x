# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"oif.h"

# OIF_WPHDR -- Write header of pixel storage file.  Called when the file
# is created.  The pixfile header is the image header structure, up to
# and including the field IM_PIXFILE.  In this case, however, the MAGIC
# field contains the string "impix", and the PIXFILE field contains the
# pathname of the header file.
# 
# Since the pixfile header contains the name of the associated header file,
# headerless pixfiles can be detected.  Since the header also contains a
# description of the physical characteristics of the contents of the file
# (not all of this info is valid at the time the header is written), an
# image which has lost its header can in principle be recovered.

procedure oif_wphdr (pfd, im, hdrfile)

int	pfd			# pixfile file descriptor
pointer	im			# image descriptor
char	hdrfile[ARB]		# filename of header file

pointer	sp, pixhdr
errchk	seek, write

begin
	call smark (sp)
	call salloc (pixhdr, LEN_IMDES + LEN_IMHDR, TY_STRUCT)

	call amovi (Memi[im], Memi[pixhdr], LEN_IMDES + LEN_IMHDR)
	call strcpy ("impix", IM_MAGIC(pixhdr), SZ_IMMAGIC)
	call fpathname (hdrfile, IM_PIXFILE(pixhdr), SZ_PATHNAME)

	call seek (pfd, BOFL)
	call write (pfd, IM_MAGIC(pixhdr), LEN_PIXHDR * SZ_STRUCT)

	call sfree (sp)
end

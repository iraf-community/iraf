# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"oif.h"

# IMWPHDR -- Write header of pixel storage file.  Called when the file
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

int procedure imwphdr (pfd, im, header_file_name)

int	pfd
pointer	im
char	header_file_name[ARB]

int	status
pointer	sp, pixhdr
int	bfwrit()

begin
	call smark (sp)
	call salloc (pixhdr, LEN_IMDES + LEN_IMHDR, TY_STRUCT)

	call amovi (Memi[im], Memi[pixhdr], LEN_IMDES + LEN_IMHDR)
	call strcpy ("impix", IM_MAGIC(pixhdr), SZ_IMMAGIC)
	call zfpath (header_file_name, IM_PIXFILE(pixhdr), SZ_PATHNAME,
	    status)

	status = bfwrit (pfd, IM_MAGIC(pixhdr), LEN_PIXHDR * SZ_STRUCT, long(1))
	call sfree (sp)
	return (status)
end

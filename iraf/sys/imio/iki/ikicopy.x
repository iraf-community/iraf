# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"iki.h"

# IKI_COPY -- Fast copy of an entire image or group of images.  This function
# is provided at the IKI level since the kernel has explicit knowledge of the
# storage format and hence may be able to copy the image by means much simpler
# and faster way than those available to the high level software.

procedure iki_copy (old, new)

char	old[ARB]		#I name of old image
char	new[ARB]		#I name of new image

int	k, n, status
pointer	sp, old_root, old_extn, new_root, new_extn
int	iki_access()
bool	streq()
errchk	syserrs

include	"iki.com"

begin
	call smark (sp)
	call salloc (old_root, SZ_PATHNAME, TY_CHAR)
	call salloc (old_extn, MAX_LENEXTN, TY_CHAR)
	call salloc (new_root, SZ_PATHNAME, TY_CHAR)
	call salloc (new_extn, MAX_LENEXTN, TY_CHAR)

	# Verify that the old image exists and determine its type.
	k = iki_access (old, Memc[old_root], Memc[old_extn], READ_ONLY)
	if (k < 0)
	    call syserrs (SYS_IKIAMBIG, old)
	else if (k == 0)
	    call syserrs (SYS_IKIIMNF, old)

	# Make sure we will not be clobbering an existing image.  Ignore
	# attempts to rename an image onto itself.

	n = iki_access (new, Memc[new_root], Memc[new_extn], 0)
	if (n > 0) {
	    if (streq (Memc[old_root], Memc[new_root]))
		if (streq (Memc[old_extn], Memc[new_extn])) {
		    call sfree (sp)
		    return
		}
	    call syserrs (SYS_IKICLOB, new)
	} else {
	    # New name is new root plus legal extn for old image.
	    call iki_parse (new, Memc[new_root], Memc[new_extn])
	    call strcpy (Memc[old_extn], Memc[new_extn], MAX_LENEXTN)
	}

	# Copy the image.
	call zcall6 (IKI_COPY(k), k, Memc[old_root], Memc[old_extn],
	    Memc[new_root], Memc[new_extn], status)
	if (status == ERR)
	    call syserrs (SYS_IKICOPY, old)

	call sfree (sp)
end

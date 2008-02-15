# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"iki.h"

# IKI_RENAME -- Rename an entire image or group of images.

procedure iki_rename (old, new)

char	old[ARB]		#I old name of image
char	new[ARB]		#I new name of image

int	k, n, status
pointer	new_root, new_extn
pointer	sp, old_root, old_extn

bool	streq()
int	iki_access()
errchk	syserrs
include	"iki.com"

begin
	call smark (sp)
	call salloc (old_root, SZ_PATHNAME, TY_CHAR)
	call salloc (old_extn, MAX_LENEXTN, TY_CHAR)
	call salloc (new_root, SZ_PATHNAME, TY_CHAR)
	call salloc (new_extn, MAX_LENEXTN, TY_CHAR)

	# Verify that the old image exists and determine its type.
	k = iki_access (old, Memc[old_root], Memc[old_extn], 0)
	if (k < 0)
	    call syserrs (SYS_IKIAMBIG, old)
	else if (k == 0)
	    call syserrs (SYS_IKIIMNF, old)

	# Determine if the old image exists.  New name is new root plus
	# legal extn for old image.

	n = iki_access (new, Memc[new_root], Memc[new_extn], 0)
	if (n <= 0)
	    call iki_parse (new, Memc[new_root], Memc[new_extn])

	# If an extension was given for the new image, verify that it is a
	# valid extension for an image of the same type as the old image.
	# We cannot change the image type in a rename operation.

	if (Memc[new_extn] != EOS) {
	    call zcall5 (IKI_ACCESS(k), k, Memc[new_root], Memc[new_extn],
		NEW_FILE, status)
	    if (status == NO)
		call strcpy (Memc[old_extn], Memc[new_extn], MAX_LENEXTN)
	} else
	    call strcpy (Memc[old_extn], Memc[new_extn], MAX_LENEXTN)

	# Make sure we will not be clobbering an existing image.  Renaming
	# an image onto itself is ok; what it means to do this is up to
	# the specific image kernel.

	if (n > 0) {
	    if (streq (Memc[old_root], Memc[new_root]) &&
		streq (Memc[old_extn], Memc[new_extn]))
		; # rename x -> x; let kernel decide what to do
	    else
		call syserrs (SYS_IKICLOB, new)
	}

	# Rename the image.
	call zcall6 (IKI_RENAME(k), k, Memc[old_root], Memc[old_extn],
	    Memc[new_root], Memc[new_extn], status)
	if (status == ERR)
	    call syserrs (SYS_IKIRENAME, old)
	
	call sfree (sp)
end

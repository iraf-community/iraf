# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"iki.h"

# IKI_DELETE -- Delete an image or group of images.

procedure iki_delete (image)

char	image[ARB]		#I name of image

int	k, status
pointer	sp, root, extn
int	iki_access()
bool	fnullfile()

errchk	syserrs
include	"iki.com"

begin
	if (fnullfile (image))
	    return

	call smark (sp)
	call salloc (root, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, MAX_LENEXTN, TY_CHAR)

	# Verify that the image exists and determine its type.
	k = iki_access (image, Memc[root], Memc[extn], 0)
	if (k < 0)
	    call syserrs (SYS_IKIAMBIG, image)
	else if (k == 0)
	    call syserrs (SYS_IKIIMNF, image)

	# Delete the image.
	call zcall4 (IKI_DELETE(k), k, Memc[root], Memc[extn], status)
	if (status == ERR)
	    call syserrs (SYS_IKIDEL, image)
	
	call sfree (sp)
end

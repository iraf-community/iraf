# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"iki.h"

# IKI_DELETE -- Delete an image or group of images.

procedure iki_delete (image)

char	image[ARB]		# name of image
int	k, status
pointer	sp, root, extn
bool	fnullfile()
int	iki_access()
include	"iki.com"
errchk	syserrs

begin
	if (fnullfile (image))
	    return

	call smark (sp)
	call salloc (root, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, MAX_LENEXTN, TY_CHAR)

	# Verify that the image exists and determine its type.
	k = iki_access (image, Memc[root], Memc[extn], 0)
	if (k <= 0)
	    call syserrs (SYS_IKIIMNF, image)

	# Delete the image.
	call zcall3 (IKI_DELETE(k), Memc[root], Memc[extn], status)
	if (status == ERR)
	    call syserrs (SYS_IKIDEL, image)
	
	call sfree (sp)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"idb.h"

# IMGETD -- Get an image header parameter of type double floating.  If the
# named parameter is a standard parameter return the value directly,
# else scan the user area for the named parameter and decode the value.

double procedure imgetd (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

int	ip
double	dval
pointer	sp, sval
int	ctod()
errchk	syserrs, imgstr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)

	ip = 1
	call imgstr (im, key, Memc[sval], SZ_LINE)
	if (ctod (Memc[sval], ip, dval) == 0)
	    call syserrs (SYS_IDBTYPE, key)

	call sfree (sp)
	return (dval)
end

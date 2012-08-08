# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"idb.h"

# IMGETB -- Get an image header parameter of type boolean.  False is returned
# if the parameter cannot be found or if the value is not true.

bool procedure imgetb (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

pointer	rp
pointer	idb_findrecord()

begin
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)
	else
	    return (Memc[rp+IDB_ENDVALUE-1] == 'T')
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	"idb.h"

# IMRENF -- Rename a user field keyword. It is an error if the
# named field does not exist.

procedure imrenf (im, oldkey, newkey)

pointer	im			# image descriptor
char	oldkey[ARB]		# old keyword
char	newkey[ARB]		# new keyword

int	off
pointer	rp, sp, keyname
int	idb_kwlookup(), idb_findrecord(), stridxs()
errchk	syserrs

begin
	call smark (sp)
	call salloc (keyname, SZ_FNAME, TY_CHAR)

	# FITS format requires that the keyword name be upper case.
	call strcpy (oldkey, Memc[keyname], IDB_SZFITSKEY)
	call strupr (Memc[keyname])

	# Cannot delete standard header keywords.
	if (idb_kwlookup (oldkey) > 0)
	    call syserrs (SYS_IDBNODEL, oldkey)

	# Verify that the named user field exists.
	if (idb_findrecord (im, oldkey, rp) <= 0)
	    call syserrs (SYS_IDBDELNXKW, oldkey)
	
	# Rename the keyword.
	call sprintf (Memc[keyname], IDB_SZFITSKEY, "%-8.8s")
	    call pargstr (newkey)
	call strupr (Memc[keyname])
	call amovc (Memc[keyname], Memc[rp], 8)
	
	call sfree (sp)
end

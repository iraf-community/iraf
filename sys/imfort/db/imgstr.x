# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"idb.h"

# IMGSTR -- Get an image header parameter of type string.  If the named
# parameter is a standard parameter return the value directly, else scan
# the user area for the named parameter and decode the value.

procedure imgstr (im, key, outstr, maxch)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
char	outstr[ARB]		# output string to receive parameter value
int	maxch

pointer	rp
int	ip, op
int	idb_getstring(), idb_findrecord(), ctowrd(), strlen()
errchk	syserrs

begin
	# Check for a standard header parameter first.
	if (idb_getstring (im, key, outstr, maxch) != ERR)
	    return

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

	ip = IDB_STARTVALUE
	if (ctowrd (Memc[rp], ip, outstr, maxch) > 0) {
	    # Strip trailing whitespace.
	    op = strlen (outstr)
	    while (op > 0 && (IS_WHITE(outstr[op]) || outstr[op] == '\n'))
		op = op - 1
	    outstr[op+1] = EOS
	} else
	    outstr[1] = EOS
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"idb.h"

# IMPSTR -- Put an image header parameter of type string.  If the named
# parameter is a standard parameter of type other than string, decode the
# string and set the binary value of the parameter.  If the parameter is
# a nonstandard one we can do a simple string edit, since user parameters
# are stored in the user area in string form.  The datatype of the parameter
# must be preserved by the edit, i.e., parameters of actual datatype string
# must be quoted and left justified and other parameters must be unquoted
# and right justified in the value field.

procedure impstr (im, key, value)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
char	value[ARB]		# new parameter value

pointer	rp, ip, vp
int	ncols, n, i
bool	string_valued
int	idb_putstring(), idb_findrecord(), strlen()
errchk	syserrs

begin
	# Check for a standard header parameter first.
	if (idb_putstring (im, key, value) != ERR)
	    return

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

	# Determine the actual datatype of the parameter.  String valued
	# parameters will have an apostrophe in the first nonblank column
	# of the value field.

	string_valued = false
	for (ip=IDB_STARTVALUE;  ip <= IDB_ENDVALUE;  ip=ip+1)
	    if (Memc[rp+ip-1] == '\'') {
		string_valued = true
		break
	    }

	vp = rp + IDB_STARTVALUE - 1
	n  = strlen (value)

	# If we have a long string value, give it the whole card.
	ncols = IDB_ENDVALUE - IDB_STARTVALUE + 1
	if (string_valued && n > 21 - 3)
	    ncols = IDB_RECLEN - IDB_STARTVALUE + 1

	# Blank fill the value field.
	do i = 1, ncols
	    Memc[vp+i-1] = ' '

	# Encode the new value of the parameter in a field of width 21
	# (or larger in the case of long string values) including a leading
	# blank and the quotes if string valued.

	if (string_valued) {
	    n = min (ncols - 3, n)
	    Memc[vp+2-1] = '\''
	    call amovc (value, Memc[vp+3-1], n)
	    Memc[vp+ncols-1] = '\''
	} else {
	    n = min (ncols - 1, n)
	    call amovc (value, Memc[vp+ncols-1-n+1], n)
	}
end

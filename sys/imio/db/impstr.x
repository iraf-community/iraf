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

int	ch, i
bool	string_valued
pointer	rp, ip, op, sp, val, start
int	idb_putstring(), idb_findrecord()
errchk	syserrs

begin
	call smark (sp)
	call salloc (val, SZ_LINE, TY_CHAR)

	# Check for a standard header parameter first.
	if (idb_putstring (im, key, value) != ERR) {
	    call sfree (sp)
	    return
	}

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

		# Clear the previous value up to the second quote.
		do i = ip, 80 {
		    ch = Memc[rp+i]
		    if (ch == '\n')
			break
		    Memc[rp+i] = ' '
		    if (ch == '\'')
			break
		}

		break
	    }

	# Encode the new value of the parameter.
	if (string_valued) {
	   call sprintf (Memc[val], SZ_LINE, " '%-0.68s%11t'%21t")
		call pargstr (value)
	} else {
	    call sprintf (Memc[val], SZ_LINE, "%21s")
		call pargstr (value)
	}

	# Update the parameter value.
	op = rp + IDB_STARTVALUE - 1
	start = op
	for (ip=val;  Memc[ip] != EOS && Memc[op] != '\n';  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}

	# If writing a string make sure the closing quote is written and
	# blank fill any leftover data from the old string value.  Don't
	# overwrite data with the closing quote, omit the closing quote if
	# it would overwrite a data (nonblank) character.

	if (string_valued) {
	    while (op > start && Memc[op-1] == ' ')
		op = op - 1
	    if (Memc[op-1] != '\'' && Memc[op] == ' ') {
		Memc[op] = '\''
		op = op + 1
	    }
	    for (ch=Memc[op];  ch != EOS && ch != '\n';  ch=Memc[op])
		if (ch == '/')		# comment field
		    break
		else {
		    Memc[op] = ' '
		    op = op + 1
		}
	}

	call sfree (sp)
end

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

pointer	im			#I image descriptor
char	key[ARB]		#I parameter to be set
char	value[ARB]		#I new parameter value

bool	string_valued
int	nchars, ch, i
pointer	rp, ip, op, sp, val, start, text, cmmt
int	idb_putstring(), idb_findrecord(), idb_filstr()
errchk	syserrs

begin
	call smark (sp)
	call salloc (val, SZ_LINE, TY_CHAR)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (cmmt, SZ_LINE, TY_CHAR)

	# Filter the value string to remove any undesirable characters.
	nchars = idb_filstr (value, Memc[text], SZ_LINE)

	# Check for a standard header parameter first.
	if (idb_putstring (im, key, Memc[text]) != ERR) {
	    call sfree (sp)
	    return
	}

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

	# Determine the actual datatype of the parameter.  String valued
	# parameters will have an apostrophe in the first nonblank column
	# of the value field.  Skip the value and treat the rest of
	# the line as a comment to be preserved.

	string_valued = false
	for (ip=IDB_STARTVALUE;  ip <= IDB_ENDVALUE;  ip=ip+1) {
	    # Skip leading whitespace.
	    for (; Memc[rp+ip-1] == ' '; ip=ip+1)
		;

	    if (Memc[rp+ip-1] == '\'') {
		# Skip string value.
		do i = ip, IDB_RECLEN {
		    ch = Memc[rp+i]
		    if (ch == '\n')
			break
		    Memc[rp+i] = ' '
		    if (ch == '\'')
			break
		}

		string_valued = true
		break

	    } else {
		# Skip numeric value.
		do i = ip, IDB_RECLEN {
		    ch = Memc[rp+i-1]
		    if (ch == '\n' || ch == ' ' || ch == '/')
			break
		    Memc[rp+i-1] = ' '
		}
		break
	    }
	}

	# Skip whitespace before any comment.
	for (ip = i; Memc[rp+ip-1] == ' '; ip=ip+1)
	    ;

	# Save comment.  Include a leading space and add a / if missing.
	Memc[cmmt] = ' '
	for (i = 1; Memc[rp+ip-1] != '\n'; ip=ip+1) {
	    if (i == 1 && Memc[rp+ip-1] != '/') {
		Memc[cmmt+i] = '/'
		i = i + 1
	    }
	    Memc[cmmt+i] = Memc[rp+ip-1]
	    Memc[rp+ip-1] = ' '
	    i = i + 1
	}
	Memc[cmmt+i] = EOS

	# Encode the new value of the parameter.
	if (string_valued) {
	   call sprintf (Memc[val], SZ_LINE, " '%-0.68s%11t'%22t%-0.68s")
		call pargstr (Memc[text])
		call pargstr (Memc[cmmt])
	} else {
	    call sprintf (Memc[val], SZ_LINE, "%21s%-0.68s")
		call pargstr (Memc[text])
		call pargstr (Memc[cmmt])
	}

	# Update the parameter value.
	op = rp + IDB_STARTVALUE - 1
	start = op
	for (ip=val;  Memc[ip] != EOS && Memc[op] != '\n';  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}

	call sfree (sp)
end

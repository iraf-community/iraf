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
pointer	rp, ip, op, sp, val, start, text
int	idb_putstring(), idb_findrecord(), idb_filstr()
errchk	syserrs

begin
	call smark (sp)
	call salloc (val, SZ_LINE, TY_CHAR)
	call salloc (text, SZ_LINE, TY_CHAR)

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
	# of the value field.

	string_valued = false
	for (ip=IDB_STARTVALUE;  ip <= IDB_ENDVALUE;  ip=ip+1)
	    if (Memc[rp+ip-1] == '\'') {
		# String valued keyword.  Clear the previous value up to
		# the second quote.

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
		# Numeric keyword.  Clear any trailing characters from the
		# previous value where trailing characters are those beyond
		# column 31, ending with either a comment or the end of card.

		do i = IDB_ENDVALUE, IDB_RECLEN {
		    ch = Memc[rp+i]
		    if (ch == '\n' || ch == ' ' || ch == '/')
			break
		    Memc[rp+i] = ' '
		}
	    }

	# Encode the new value of the parameter.
	if (string_valued) {
	   call sprintf (Memc[val], SZ_LINE, " '%-0.68s%11t'%21t")
		call pargstr (Memc[text])
	} else {
	    call sprintf (Memc[val], SZ_LINE, "%21s")
		call pargstr (Memc[text])
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

include <ctype.h>		# for IS_ALNUM

# tbfile -- get table and file name
# This routine takes a table name as specified by a user and returns
# the full table name, the full file name, and the filename extension
# (including the dot; e.g. ".tab").  The filename extension may be the
# null string if the file is a text table.  The file name will be a
# subset of the table name, as the table name may include a bracketed
# expression giving EXTNAME or HDU number or table name in CDF file.
#
# Phil Hodge, 27-Jun-1995  Subroutine created.
# Phil Hodge, 29-Sep-1997  No longer necessary to enclose extname expression
#			in brackets, as the brackets are now included.
# Phil hodge, 16-Apr-1999  Remove ttype from calling sequence of tbparse.

procedure tbfile (input, tabname, filename, extn, maxch)

char	input[ARB]	# i: input table name
char	tabname[maxch]	# o: full table name
char	filename[maxch]	# o: name of file containing table
char	extn[maxch]	# o: filename extension, including '.'
int	maxch		# i: size of strings
#--
pointer sp
pointer fname		# full file name
pointer brackets	# for CDF or HDU name or number, and/or selectors
int	hdu		# returned by tbparse and ignored
int	dotloc		# location of last '.' in file name
int	i
int	strlen(), access()
int	tbparse()
bool	strne()
errchk	tbparse, tbtext

begin
	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (brackets, SZ_LINE, TY_CHAR)

	# Separate filename from any bracketed expression (such as
	# EXTNAME or HDU number) that may be present.
	if (tbparse (input, Memc[fname], Memc[brackets], SZ_LINE, hdu) < 1) {
	    tabname[1] = EOS
	    filename[1] = EOS
	    extn[1] = EOS
	    call sfree (sp)
	    return
	}

	# Append default extension (if appropriate) to get full file name.
	# A text table need not have an extension, so first check whether
	# a file of the given name exists.  If not, then append extension.
	if (access (Memc[fname], 0, 0) == NO &&
		strne (input, "STDIN") && strne (input, "STDOUT"))
	    call tbtext (Memc[fname], Memc[fname], SZ_LINE)

	# At this point we have the full file name; copy it to output.
	call strcpy (Memc[fname], filename, maxch)

	# Append bracketed expression (if present) to get full table name,
	# and copy it to output.
	call strcpy (Memc[fname], tabname, maxch)
	if (Memc[brackets] != EOS)
	    call strcat (Memc[brackets], tabname, maxch)

	# Search for a filename extension.  Look for a dot that is not
	# followed by any special character.
	dotloc = 0				# initial value
	do i = strlen (Memc[fname]), 1, -1 {
	    if (Memc[fname+i-1] == '.') {	# found it
		dotloc = i
		break
	    }
	    if (!IS_ALNUM(Memc[fname+i-1]))	# stop at first special char
		break
	}

	# If the file name includes an extension, copy it to output.
	if (dotloc > 0)
	    call strcpy (Memc[fname+dotloc-1], extn, maxch)
	else
	    extn[1] = EOS

	call sfree (sp)
end

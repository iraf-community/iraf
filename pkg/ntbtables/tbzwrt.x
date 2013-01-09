include <ctype.h>
include <tbset.h>
include "tbtables.h"
include "tbltext.h"

define	COLWIDTH	10	# width for holding print format for a column

# tbzwrt -- write data values to a text file
# The table data are written from memory to a temporary file, the original
# text table is deleted, and then the temp file is renamed to the name
# of the original text table.  The file is closed by this routine, and
# TB_FILE(tp) is set to NULL.  If the output file is STDOUT or STDERR,
# however, the data are just written to that fd.
# A string will be enclosed in quotes if the string contains a blank or tab
# or if it begins with a number or plus or minus.  The latter is necessary
# in case the table rows are reordered, putting this string in the first
# row, because without quotes it would appear to be a numeric column.
#
# If no change has been made to the table, this routine returns without
# doing anything.
#
# Phil Hodge, 25-Mar-1992  Subroutine created.
# Phil Hodge, 20-Jul-1992  Don't quote string just because it begins with digit.
# Phil Hodge, 25-Nov-1994  Don't quote if only leading or trailing blanks.
# Phil Hodge,  2-Dec-1994  Include test on pform longer than SZ_OBUF.
# Phil Hodge,  3-Apr-1995  Check TB_MODIFIED.
# Phil Hodge,  2-Jan-1996  Quote blank strings; write INDEFI for undefined int.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.
# Phil Hodge, 12-Apr-1999  Check for STDERR, in addition to STDOUT.
# Phil Hodge, 21-Apr-1999  Print each column one at a time, rather than
#		using one fprintf with all the print formats concatenated;
#		this is to avoid the line length limit imposed by SZ_OBUF.
# Phil Hodge, 15-Jun-1999  Modify for table with explicit column definitions.

procedure tbzwrt (tp)

pointer tp			# i: pointer to table descriptor
#--
pointer sp
pointer temp			# scratch for name of temporary table
pointer cbuf			# buffer for output string
pointer colname			# for comparing column name with "c<n>"
pointer cp			# pointer to column descriptor
int	fd			# fd for temporary table
int	key			# loop index for keyword number
int	row_1			# row number minus one
int	ncols			# number of columns
int	colnum			# column number
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
int	i			# loop index
int	istart, iend		# limits on i when looking for embedded blanks
bool	to_stdout		# is output file STDOUT?
bool	quote			# whitespace in string?  then enclose in quotes
char	blank			# ' ', as an argument to stridx
int	stridx()
int	strlen(), open()
bool	streq()

begin
	if (!TB_MODIFIED(tp))
	    return

	blank = ' '

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (cbuf, SZ_LINE, TY_CHAR)

	ncols = TB_NCOLS(tp)

	# If the output file is STDOUT or STDERR, we just write to it.
	if (streq (TB_NAME(tp), "STDOUT")) {
	    to_stdout = true
	    fd = STDOUT
	} else if (streq (TB_NAME(tp), "STDERR")) {
	    to_stdout = true
	    fd = STDERR
	} else {
	    to_stdout = false
	    # Create temporary table (text file).
	    call mktemp ("tmp$texttbl", Memc[temp], SZ_FNAME)
	    fd = open (Memc[temp], NEW_FILE, TEXT_FILE)
	}

	# Check whether the table has been "converted" from simple format
	# to explicit, by setting a column name or units.  If any column
	# name differs from "c<N>" (case insensitive; any N, not just the
	# current column number), or if units have been specified for any
	# column, the table subtype will be reset to explicit column def.
	if (TB_SUBTYPE(tp) != TBL_SUBTYPE_EXPLICIT) {
	    call salloc (colname, SZ_COLNAME, TY_CHAR)
	    do colnum = 1, TB_NCOLS(tp) {
		cp = TB_COLINFO(tp,colnum)
		if (COL_UNITS(cp) != EOS) {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
		    break
		}
		call strcpy (COL_NAME(cp), Memc[colname], SZ_COLNAME)
		call strlwr (Memc[colname])
		if (Memc[colname] != 'c') {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
		    break
		} else if (Memc[colname+1] == EOS || Memc[colname+1] == '0') {
		    # A column name for a simple text table is never just "c"
		    # without a number, and the number never begins with "0".
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
		    break
		} else {
		    do i = 2, SZ_COLNAME {
			if (Memc[colname+i-1] == EOS)
			    break
			if (!IS_DIGIT(Memc[colname+i-1])) {
			    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
			    break
			}
		    }
		}
	    }
	}

	# If the table has explicit column definitions, write them.
	if (TB_SUBTYPE(tp) == TBL_SUBTYPE_EXPLICIT) {

	    do colnum = 1, TB_NCOLS(tp) {

		cp = TB_COLINFO(tp,colnum)

		call fprintf (fd, "#c ")

		quote = (stridx (blank, COL_NAME(cp)) > 0)
		if (quote) {
		    call fprintf (fd, "\"%s\"")
		} else {
		    call fprintf (fd, "%s")
		}
		    call pargstr (COL_NAME(cp))

		if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
		    call fprintf (fd, " d")
		} else if (COL_DTYPE(cp) == TBL_TY_INT) {
		    call fprintf (fd, " i")
		} else if (COL_DTYPE(cp) < 0) {
		    call fprintf (fd, " ch*%d")
			call pargi (-COL_DTYPE(cp))
		} else {
		    call fprintf (fd, " ch*1024")
		}

		call fprintf (fd, " %s")
		    call pargstr (COL_FMT(cp))

		quote = (stridx (blank, COL_UNITS(cp)) > 0)
		if (quote) {
		    call fprintf (fd, " \"%s\"")
		} else {
		    call fprintf (fd, " %s")
		}
		    call pargstr (COL_UNITS(cp))
		call fprintf (fd, "\n")
	    }
	}

	# If there are keywords, write them.
	if (TB_KEYLIST_PTR(tp) != NULL) {
	    do key = 1, TB_NPAR(tp) {
		call fprintf (fd, "%s\n")
		    call pargstr (Memc[TB_KEYWORD(tp,key)])
	    }
	}

	# Write the comment buffer to the output file.
	if (TB_COMMENT(tp) != NULL) {
	    if (Memc[TB_COMMENT(tp)] != EOS)
		call putline (fd, Memc[TB_COMMENT(tp)])
	}

	# Print each row to the file.
	do row_1 = 0, TB_NROWS(tp) - 1 {		# zero indexed

	    # Print each column in the current row.
	    do colnum = 1, ncols {

		cp = TB_COLINFO(tp,colnum)

		if (colnum > 1)			# separator between columns
		    call fprintf (fd, " ")

		call fprintf (fd, COL_FMT(cp))		# use this format

		# Now call the appropriate parg routine.
		if (COL_DTYPE(cp) == TY_DOUBLE) {

		    call pargd (Memd[COL_OFFSET(cp) + row_1])

		} else if (COL_DTYPE(cp) == TY_INT) {

		    if (IS_INDEFI (Memi[COL_OFFSET(cp) + row_1]))
			call pargstr ("INDEFI")
		    else
			call pargi (Memi[COL_OFFSET(cp) + row_1])

		} else {				# string

		    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
		    ip = row_1 * lenstr			# offset to element

		    # Check for embedded whitespace.
		    quote = false			# initial value

		    # istart and iend are zero indexed
		    istart = 0
		    while (IS_WHITE(Memc[COL_OFFSET(cp)+ip+istart]))
			istart = istart + 1		# skip leading blanks
		    iend = strlen (Memc[COL_OFFSET(cp)+ip]) - 1
		    if (istart > iend)
			quote = true			# null or all blank
		    while (iend > istart &&
			    IS_WHITE(Memc[COL_OFFSET(cp)+ip+iend])) {
			iend = iend - 1			# skip trailing blanks
		    }

		    do i = istart, iend {		# zero indexed
			if (IS_WHITE(Memc[COL_OFFSET(cp)+ip+i])) {
			    quote = true
			    break
			}
		    }

		    if (quote) {
			Memc[cbuf] = '"'
			Memc[cbuf+1] = EOS
			call strcat (Memc[COL_OFFSET(cp)+ip], Memc[cbuf],
					SZ_LINE)
			call strcat ("\"", Memc[cbuf], SZ_LINE)
			call pargstr (Memc[cbuf])
		    } else {
			call pargstr (Memc[COL_OFFSET(cp)+ip])
		    }
		}
	    }
	    call fprintf (fd, "\n")
	}

	call close (fd)

	if (!to_stdout) {
	    # Close and delete the original text table, and rename the
	    # new (temporary) file back to the name of the original.
	    call close (TB_FILE(tp))
	    call delete (TB_NAME(tp))
	    call rename (Memc[temp], TB_NAME(tp))
	}
	TB_FILE(tp) = NULL			# to indicate that it's closed

	call sfree (sp)
end

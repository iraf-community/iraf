include <mach.h>		# for MAX_DIGITS, NDIGITS_DP and SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tbltext.h"

define	MAX_WIDTH_INT	11		# maximum width of integer field

# tbzrds -- read a (simple) text file into memory
#
# When the first non-comment line is read, tbzcol is called to figure out
# the data types, to get initial info about precision, and to create columns
# and allocate memory for column values.
#
# The input buffer buf will likely already contain the first data line of
# the file when this routine is called.  (If not, buf will have been set
# to EOS.)  This is because the beginning of the file is read to determine
# whether it is a simple text table or one with explicit column definitions.
#
# For each data line (including the first), tbzmem is called to read values
# into memory.  It updates the column widths in case values are wider than in
# the first row.  tbzmem also reallocates buffers, if necessary, and updates
# the number of rows.
#
# The print format is set by this routine, and maximum values are set
# for the widths of double precision and integer columns.  COL_LEN is
# also set here, since columns are defined based on values in the first
# row, and the data type or width of a character column can change after
# the first row.
#
# This version is for simple text tables.  The version that is appropriate
# for a table with explicit column definitions is tbzrdx.
#
# Phil Hodge,  7-Jun-1999  Subroutine created, based on tbzopn.
# Phil Hodge,  5-Aug-1999  Assign COL_NELEM = 1.

procedure tbzrds (tp, buf, line, line_type)

pointer tp		# i: pointer to table descriptor
char	buf[ARB]	# io: buffer for input line
int	line		# io: line number in input file
int	line_type	# io: type of line read by tbzlin
#--
pointer sp
pointer wid		# pointer to array of values of width
pointer prec		# pointer to array of values of precision
pointer fmt_code	# pointer to array of format codes
pointer cp		# pointer to column descriptor
char	pform[SZ_COLFMT]	# print format
int	fd		# fd for the file
int	width		# width of field for printing
int	precision	# for print format
int	row		# row number (can be from more than one line)
int	colnum		# column number
bool	first		# is current line the first non-comment line?
bool	done
int	nchar, tbzlin()	# reads a line from the file
errchk	tbcfmt, tbzlin, tbzcol, tbzmem, tbzkey

begin
	call smark (sp)

	# Read the file into memory.
	fd = TB_FILE(tp)
	first = true
	row = 0

	done = false

	# If the input buffer is empty, read the first line.
	if (buf[1] == EOS) {
	    nchar = tbzlin (fd, buf, SZ_TEXTBUF, line, line_type)
	    if (nchar == EOF)
		done = true
	}

	while (!done) {

	    if (line_type == COMMENT_LINE) {

		call tbbcmt (tp, buf)		# append to comment buffer

	    } else if (line_type == KEYWORD_LINE) {

		call tbzkey (tp, buf, 0)	# append to list of keywords

	    } else if (line_type == COLDEF_LINE) {

		call error (1,
		"tbtopn:  column definitions must precede all data lines")

	    } else if (line_type == DATA_LINE) {

		row = row + 1

		if (first) {
		    # Get initial column definitions from first row.
		    call tbzcol (tp, buf, wid, prec, fmt_code)
		    first = false
		}

		# Read data into memory.
		call tbzmem (tp, buf, row, line,
				Memi[wid], Memi[prec], Memc[fmt_code])
	    } else {
		call error (1, "tbzrds:  internal error")
	    }

	    # Read the next line.
	    nchar = tbzlin (fd, buf, SZ_TEXTBUF, line, line_type)
	    if (nchar == EOF)
		done = true
	}

	# Set print formats and data lengths.
	pform[1] = '%'
	do colnum = 1, TB_NCOLS(tp) {
	    cp = TB_COLINFO(tp,colnum)

	    width = Memi[wid+colnum-1]

	    # The format code fmt_code is used for double because it could
	    # be g or h or m, but it is ignored for int and char.
	    if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
		width = min (width, MAX_DIGITS)
		precision = Memi[prec+colnum-1]
		if (Memc[fmt_code+colnum-1] == 'g')
		    precision = max (precision, width-2) # maximum precision
		precision = min (precision, NDIGITS_DP)
		call sprintf (pform[2], SZ_COLFMT-1, "%d.%d%c")
		    call pargi (width)
		    call pargi (precision)
		    call pargc (Memc[fmt_code+colnum-1])

		COL_LEN(cp) = SZ_DOUBLE

	    } else if (COL_DTYPE(cp) == TBL_TY_INT) {
		width = min (width, MAX_WIDTH_INT)
		call sprintf (pform[2], SZ_COLFMT-1, "%dd")
		    call pargi (width)

		COL_LEN(cp) = SZ_INT32

	    } else {				# character string
		call sprintf (pform[2], SZ_COLFMT-1, "-%ds")
		    call pargi (width)

		COL_LEN(cp) = (width + SZB_CHAR-1) / SZB_CHAR
	    }

	    # Set the print format for this column.
	    call strcpy (pform, COL_FMT(cp), SZ_COLFMT)

	    COL_NELEM(cp) = 1		# ascii tables do not support arrays
	}

	# Free memory allocated by tbzcol.
	call mfree (fmt_code, TY_CHAR)
	call mfree (prec, TY_INT)
	call mfree (wid, TY_INT)

	call sfree (sp)
end

include <tbset.h>
include "tbtables.h"
include "tbltext.h"

# tbzrdx -- read an explicit-column-definitions text file into memory
#
# The input buffer buf will likely already contain a line from the input
# file, perhaps a column definition.  (If not, buf will have been set
# to EOS.)  This is because the beginning of the file is read to determine
# whether it is a simple text table or one with explicit column definitions.
#
# A new column will be created for each line of the file that contains
# an explicit column definition (i.e. each line beginning with "#c ").
#
# For each data line, tbzmex is called to read values into memory.  tbzmex
# reallocates buffers, if necessary, and updates the number of rows.
#
# This version is for text tables with explicit column definitions.  The
# version that is appropriate for a simple text table is tbzrds.
#
# Phil Hodge,  7-Jun-1999  Subroutine created, based on tbzopn.
# Phil Hodge, 24-Sep-1999  In tbbecd, call tbztyp instead of tbbtyp.

procedure tbzrdx (tp, buf, line, line_type)

pointer tp		# i: pointer to table descriptor
char	buf[ARB]	# io: buffer for input line
int	line		# io: line number in input file
int	line_type	# io: type of line read by tbzlin
#--
pointer cp		# pointer to column descriptor
char	colname[SZ_COLNAME]	# column name (read from buffer)
int	datatype		# data type code for a column
char	colfmt[SZ_COLFMT]	# print format for a column
char	colunits[SZ_COLUNITS]	# units for a column
int	fd		# fd for the file
int	row		# row number (can be from more than one line)
bool	data_read	# has a line of data been read?
bool	done
int	nchar, tbzlin()	# reads a line from the file
errchk	tbzlin, tbzmex, tbbecd, tbcadd, tbzkey

begin
	fd = TB_FILE(tp)
	row = 0
	data_read = false

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

		if (data_read) {
		    call error (1,
		"tbtopn:  column definitions must precede all data lines")
		}

		# Interpret column definition, and create a new column.
		call tbbecd (buf, colname, datatype, colfmt, colunits)
		call tbcadd (tp, cp, colname, colunits, colfmt, datatype, 1, 1)

	    } else if (line_type == DATA_LINE) {

		data_read = true
		row = row + 1

		# Read data into memory.
		call tbzmex (tp, buf, row, line)

	    } else {
		call error (1, "tbzrdx:  internal error")
	    }

	    # Read the next line.
	    nchar = tbzlin (fd, buf, SZ_TEXTBUF, line, line_type)
	    if (nchar == EOF)
		done = true
	}
end

# tbbecd -- read column definition from buffer
# This routine skips over "#c ", then reads the column name, data type,
# print format, and units from the input buffer.  Only the column name
# is required; the default data type is double.

procedure tbbecd (buf, colname, datatype, colfmt, colunits)

char	buf[ARB]		# i: buffer containing column definition
char	colname[SZ_COLNAME]	# o: column name
int	datatype		# o: data type code for column
char	colfmt[SZ_COLFMT]	# o: print format for column
char	colunits[SZ_COLUNITS]	# o: units for column
#--
char	chdtype[SZ_COLNAME]	# scratch for data type extracted from buffer
int	ip, ctowrd()
errchk	tbbftp, tbztyp

begin
	ip = 4			# skip over "#c "
	if (ctowrd (buf, ip, colname, SZ_COLNAME) < 1)
	    call error (1, "could not read column name")

	if (ctowrd (buf, ip, chdtype, SZ_COLNAME) < 1) {
	    call strcpy ("d", chdtype, SZ_COLNAME)	# default is double
	    colfmt[1] = EOS
	    colunits[1] = EOS
	} else if (ctowrd (buf, ip, colfmt, SZ_COLFMT) < 1) {
	    colfmt[1] = EOS
	    colunits[1] = EOS
	} else if (ctowrd (buf, ip, colunits, SZ_COLUNITS) < 1) {
	    colunits[1] = EOS
	}

	# Convert the format from Fortran style to SPP style.
	call tbbftp (colfmt, colfmt)

	# Convert the data type to an integer code.
	call tbztyp (chdtype, datatype)
end

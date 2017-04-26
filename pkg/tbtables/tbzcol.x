include <ctype.h>		# for IS_DIGIT
include <tbset.h>
include "tbtables.h"

# tbzcol -- create columns
# This routine looks at the values on the first input line of a text file,
# interprets the values as to data type, assigns a print format for each
# column, and creates a column descriptor.
# Since the input buffer contains the first line of the text file, we
# check in this routine to see if the file is really an SDAS format (GEIS)
# image header rather than a text table.
# The datatype value returned by tbbwrd may be TY_DOUBLE, TY_INT, or TY_CHAR;
# however, TY_CHAR will be changed to -N.

# Phil Hodge, 15-Jan-1992  Subroutine created.
# Phil Hodge,  7-Aug-1992  Remove most code that is already done in tbbwrd.
# Phil Hodge,  7-Jun-1994  For text string, don't increase width.
# Phil Hodge,  7-Jun-1999  Don't realloc TB_COLPTR, done in tbcadd instead.

procedure tbzcol (tp, buf, wid, prec, fmt_code)

pointer tp			# i: pointer to table descriptor
char	buf[ARB]		# i: buffer containing line from file
pointer wid			# o: pointer to array of values of width
pointer prec			# o: pointer to array of (width - precision)
pointer fmt_code		# o: pointer to array of format codes
#--
pointer sp
pointer word			# scratch for a word from the line
pointer cp			# pointer to column descriptor
char	cname[SZ_COLNAME]	# column name
char	units[SZ_COLUNITS]	# column units (null)
char	pform[SZ_COLFMT]	# print format
int	colnum			# column number
int	maxcols			# current maximum number of columns
int	ip			# for ctowrd
int	ip_start		# ip before calling ctowrd
int	word_width		# width of extracted word (value of ctowrd)
int	width			# full width of column (incl whitespace)
int	precision		# precision for printing
int	datatype		# data type of current word (from tbbwrd)
int	fcode			# format code returned by tbbwrd
bool	done			# loop-termination flag
int	tbbwrd(), strncmp()
errchk	realloc, tbcadd

begin
	# First check whether we have an SDAS format image header.
	if (strncmp ("SIMPLE  = ", buf, 10) == 0)
	    call error (1, "file is an image header, not a table")

	call smark (sp)
	call salloc (word, SZ_LINE, TY_CHAR)

	# Allocate buffers for column widths, precision, and format codes.
	maxcols = TB_MAXCOLS(tp)
	call malloc (wid, maxcols, TY_INT)
	call malloc (prec, maxcols, TY_INT)
	call malloc (fmt_code, maxcols, TY_CHAR)

	colnum = 0				# initial values
	ip = 1
	done = false
	pform[1] = EOS				# not set by this routine
	units[1] = EOS

	# Do for each word in the string.
	while ( !done ) {

	    ip_start = ip
	    word_width = tbbwrd (buf, ip, Memc[word], SZ_LINE,
			width, precision, datatype, fcode)

	    if (word_width < 1) {
		done = true			# we're past the last word

	    } else if (Memc[word] == ',') {
		;				# ignore commas in first line
	    } else {
		colnum = colnum + 1
		call sprintf (cname, SZ_COLNAME, "c%d")
		    call pargi (colnum)

		# This is to allow the user to increase the width by adding
		# extra space in the first row.  Subtract one for columns
		# after the first because we're going to print a space
		# between each column.
		if (colnum > 1)
		    width = max (width, ip - ip_start - 1)
		else
		    width = max (width, ip - ip_start)

		# For a character string set the data type to -N.
		# Changed by PEH on 1994 June 6; the width used to be
		# increased by 20 to allow extra space.
		if (datatype == TY_CHAR)
		    datatype = -min (width, SZ_LINE)

		# Allocate a descriptor for the column, update TB_MAXCOLS(tp),
		# and allocate & initialize a buffer for storing values.
		call tbcadd (tp, cp, cname, units, pform, datatype, 1, 1)

		# Reallocate space for column widths and precision.
		if (colnum > maxcols) {
		    maxcols = TB_MAXCOLS(tp)
		    call realloc (wid, maxcols, TY_INT)
		    call realloc (prec, maxcols, TY_INT)
		    call realloc (fmt_code, maxcols, TY_CHAR)
		}

		# Save values of width, precision and format code.  The latter
		# is really only used (by tbzopn) for h, m, and g formats.
		Memi[wid+colnum-1] = width
		Memi[prec+colnum-1] = precision
		Memc[fmt_code+colnum-1] = fcode
	    }
	}

	call sfree (sp)
end

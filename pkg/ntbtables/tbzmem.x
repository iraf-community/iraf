include <ctype.h>
include <tbset.h>
include "tbtables.h"

define	NUM_EXTRA	1000	# number of extra "rows" when reallocating

# This file contains tbzmem, tbzmex, and tbzpbt.

# tbzmem -- read values from string
# This routine reads the values out of a line from a text file and puts
# them into memory.
# The variable wid is an array (one element for each column) giving the
# width of each column in the input file.  This width will be used to set
# the print format; both the width and precision are affected.  The value
# for each column is updated by this routine whenever the width is greater
# than the value from previous rows.
# The variable line is passed to this routine only for possible use in
# error messages.
#
# The allocated number of rows (i.e. the size of the internal buffers for
# values read from the text file) will be increased if necessary.

# Phil Hodge, 15-Jan-1992  Subroutine created.
# Phil Hodge,  7-Aug-1992  Add fcode to calling sequence for tbbwrd.
# Phil Hodge,  6-Dec-1992  Add line to calling sequence for error messages.
# Phil Hodge,  7-Jun-1994  If different data type from first row, change type;
#		include fmt_code in calling sequence; possibly update
#		format code (e.g. 'g' to 'h') for type double.
# Phil Hodge, 30-Apr-1996  Replace call to tbzptt with tbzpbt (in this file).
# Phil Hodge,  7-Jun-1999  tbzmex added, based on tbzmem;
#		in tbzpbt, call tbtchs instead of tbzsiz.

procedure tbzmem (tp, buf, row, line, wid, prec, fmt_code)

pointer tp			# i: pointer to table descriptor
char	buf[ARB]		# i: buffer containing line from file
int	row			# i: row number
int	line			# i: line number in input file
int	wid[ARB]		# io: width of each column
int	prec[ARB]		# io: precision of each column
char	fmt_code[ARB]		# io: format code
#--
pointer sp
pointer word			# scratch for a word from the line
pointer message			# scratch for possible error message
pointer cp			# pointer to column descriptor
int	colnum			# column number
int	ip			# for ctowrd
int	word_width		# value returned by ctowrd (called by tbbwrd)
int	width			# actual width of current word
int	precision		# actual precision of current word
int	datatype		# data type of current word
int	fcode			# format code from tbbwrd
bool	done			# loop-termination flag
int	strncmp(), tbbwrd()
errchk	tbzpbt

begin
	call smark (sp)
	call salloc (word, SZ_LINE, TY_CHAR)

	colnum = 0				# initial values
	ip = 1
	done = false

	# Do for each word in the string.
	while ( !done ) {

	    word_width = tbbwrd (buf, ip, Memc[word], SZ_LINE,
			width, precision, datatype, fcode)

	    if (word_width > 0) {

		colnum = colnum + 1

		if (colnum > TB_NCOLS(tp)) {
		    call salloc (message, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[message], SZ_LINE,
	"column found in line %d that was not defined in first row")
			call pargi (line)
		    call error (1, Memc[message])
		}

		# Check whether the current word is too long.
		if (width > SZ_LINE-1) {
		    call salloc (message, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[message], SZ_LINE,
	"string in line %d is too long for a table; the maximum is %s")
			call pargi (line)
			call pargi (SZ_LINE-1)
		    call error (1, Memc[message])
		}

		# Update values of width and prec gotten from previous rows.
		wid[colnum] = max (wid[colnum], width)
		prec[colnum] = max (prec[colnum], precision)

		# A comma after whitespace means a column value is not given.
		if (Memc[word] != ',') {

		    # Check whether current word is consistent with
		    # the data type of the column.
		    cp = TB_COLINFO(tp,colnum)
		    if (COL_DTYPE(cp) < 0) {		# string; check length

			if (width > -COL_DTYPE(cp))
			    # Allocated width is too small; increase it.
			    call tbzt2t (tp, cp, wid[colnum])

		    } else if (datatype == TY_CHAR) {

			# Change data type to text.
			if (COL_DTYPE(cp) == TY_DOUBLE)
			    call tbzd2t (tp, cp, wid[colnum],
					prec[colnum], fmt_code[colnum])
			else if (COL_DTYPE(cp) == TY_INT)
			    call tbzi2t (tp, cp, wid[colnum])

			# Change the code for print format.
			fmt_code[colnum] = 's'

		    } else if (COL_DTYPE(cp) == TBL_TY_INT &&
				datatype == TY_DOUBLE) {

			# Change data type to double.  (but INDEF is numeric)
			if (strncmp (Memc[word], "INDEF", 5) != 0)
			    call tbzi2d (tp, cp)

		    }

		    # Possibly update the format code:  d --> g --> m --> h
		    if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
			if (fcode == 'h') {
			    if (fmt_code[colnum] == 'g')
				prec[colnum] =
					max (precision, prec[colnum]-6, 1)
			    fmt_code[colnum] = fcode
			} else if (fcode == 'm' && fmt_code[colnum] != 'h') {
			    if (fmt_code[colnum] == 'g')
				prec[colnum] =
					max (precision, prec[colnum]-4, 1)
			    fmt_code[colnum] = fcode
			} else if (fcode == 'g' && fmt_code[colnum] == 'd') {
			    fmt_code[colnum] = fcode
			}
		    }

		    # Save the value in the buffer for this column.
		    call tbzpbt (tp, cp, row, Memc[word])
		}

		# If a comma was used as a separator, skip over it.
		if (Memc[word+word_width] == ',')
		    ip = ip + 1
	    } else {
		done = true			# we're past the last word
	    }
	}

	call sfree (sp)
end

# tbzmex -- read values from string
# This routine reads the values out of a line from a text file and puts
# them into memory.
#
# This version is for tables with explicit column definitions.
#
# The variable line is passed to this routine only for possible use in
# error messages.

procedure tbzmex (tp, buf, row, line)

pointer tp			# i: pointer to table descriptor
char	buf[ARB]		# i: buffer containing line from file
int	row			# i: row number
int	line			# i: line number in input file
#--
pointer sp
pointer word			# scratch for a word from the line
pointer message			# scratch for possible error message
pointer cp			# pointer to column descriptor
int	colnum			# column number
bool	done			# loop-termination flag
# word_width is returned by ctowrd and will count the enclosing quotes,
# if there are any, while len is the actual length of the column entry
int	word_width, len, strlen()
int	ip, ctowrd()
bool	strne()
errchk	tbzpbt

begin
	call smark (sp)
	call salloc (word, SZ_LINE, TY_CHAR)

	colnum = 0				# initial values
	ip = 1
	done = false

	# Do for each word in the string.
	while ( !done ) {

	    word_width = ctowrd (buf, ip, Memc[word], SZ_LINE)

	    if (word_width > 0) {

		len = strlen (Memc[word])

		colnum = colnum + 1

		if (colnum > TB_NCOLS(tp)) {
		    call salloc (message, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[message], SZ_LINE,
	"column was found that was not explicitly defined (line %d)")
			call pargi (line)
		    call error (1, Memc[message])
		}

		# Check whether the current word is too long.
		if (len > SZ_LINE-1) {
		    call salloc (message, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[message], SZ_LINE,
	"string in line %d is too long for a table; the maximum is %s")
			call pargi (line)
			call pargi (SZ_LINE-1)
		    call error (1, Memc[message])
		}

		# A comma after whitespace means a column value was not given.
		if (strne (Memc[word], ',')) {

		    # Check whether current word is consistent with
		    # the data type of the column.
		    cp = TB_COLINFO(tp,colnum)

		    # If a comma was used as a separator, trim it.
		    if (COL_DTYPE(cp) > 0 && Memc[word+len-1] == ',')
			Memc[word+len-1] = EOS

		    # Save the value in the buffer for this column.
		    call tbzpbt (tp, cp, row, Memc[word])
		}

	    } else {
		done = true			# we're past the last word
	    }
	}

	call sfree (sp)
end

# tbzpbt -- copy text string into internal buffer
# This routine is based on tbzptt.  The latter calls tbtwer to ensure
# that the buffers are large enough (i.e. TB_ALLROWS >= rownum) and to
# update TB_NROWS.  We want to avoid tbtwer because it sets TB_MODIFIED
# to true.  tbzpbt reallocates the buffers if rownum > TB_ALLROWS, and
# it updates TB_NROWS if appropriate.

procedure tbzpbt (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
char	buffer[ARB]		# i: value to be put
#--
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc
long	lval			# so we can use ctol
int	ctod(), ctol()
errchk	tbtchs

begin
	# Increase the size of buffers for storing column values, if necessary.
	# (TB_MAXPAR remains unchanged.)
	if (rownum > TB_ALLROWS(tp))
	    call tbtchs (tp, -1, -1, -1, rownum + NUM_EXTRA)

	# If we're writing beyond EOF, update TB_NROWS.
	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)

	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    ip = 1
	    if (ctod (buffer, ip, Memd[COL_OFFSET(cp) + rownum - 1]) < 1)
		Memd[COL_OFFSET(cp) + rownum - 1] = INDEFD

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ip = 1
	    if (ctol (buffer, ip, lval) > 0)
		Memi[COL_OFFSET(cp) + rownum - 1] = lval
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI

	} else {				# string
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (buffer, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

include <ctype.h>	# for IS_WHITE
include <tbset.h>
include "tprint.h"

# tpr_plain_pr -- print contents of table
# This version simply prints the table data.  The corresponding procedure
# that prints in TeX/LaTeX format is tpr_latex_pr.
# It may be that all the columns that are to be printed will not fit
# on one page, in which case they are printed in sections:  all the rows
# are printed for the first set of columns, then all the rows for the next
# set, etc.
#
# Phil Hodge,  5-Oct-1987  Subroutine created
# Phil Hodge,  7-Oct-1987  prt_row:  use different buffer for each data type.
# Phil Hodge, 12-Feb-1988  Include option to align columns with header
# Phil Hodge, 30-Mar-1988  Use a column to control spacing of printout.
# Phil Hodge,  6-Jan-1989  Call tpr_break for new page, also after tpr_cnames_pr
# Phil Hodge,  2-Apr-1993  In prt_row, include short datatype.
# Phil Hodge,  5-Jul-1993  Include option to print column units.
# Phil Hodge, 16-Feb-1995  Print "#" before header lines to comment them out.
# Phil Hodge, 26-Mar-1998  Add orig_row to calling sequence, use in prt_row;
#		remove showrow from calling sequences of
#		tpr_break and tpr_cnames_pr.
# Phil Hodge, 18-Jan-1999  Get boolean as string, to preserve indef values.
# Phil Hodge, 30-Mar-1999  Delete declaration of bbuf from prt_row.
# Phil Hodge,  9-Aug-1999  Print all array elements;
#		move code for printing blank lines into prt_row;
#		change the calling sequence of tpr_space;
#		delete data_fmt and subroutine tpr_g_fmt.
# Phil Hodge,  3-Jul-2000  In tpr_pfmt, use "%-s" format for the last column
#		on a page, if the column should be left justified.
# Phil Hodge,  2-Nov-2000  Remove the restriction that no more than MAXCOLS
#		columns can be printed on one page.

procedure tpr_plain_pr (tp, cptr, nprint, s_cp, lgroup,
		range_string, pagewidth, pagelength,
		showrow, orig_row, showhdr, showunits, align)

pointer tp			# i: pointer to table descriptor
pointer cptr[nprint]		# i: array of pointers to column descriptors
int	nprint			# i: number of columns to print
pointer s_cp			# i: pointer to column to control spacing
int	lgroup			# i: print blank line after this many lines
char	range_string[ARB]	# i: string which gives ranges of row numbers
int	pagewidth		# i: page width
int	pagelength		# i: number of data lines per page
bool	showrow			# i: true if row number is to be printed
bool	orig_row		# i: show row number from underlying table?
bool	showhdr			# i: print column names?
bool	showunits		# i: print column units?
bool	align			# i: override print fmt to align col & name?
#--
pointer sp
pointer colptr			# ptr to descriptors for columns on current page
pointer lenfmt			# ptr to lengths of print fmt for cols on page
pointer cn_fmt			# formats for printing column values
char	rn_fmt[SZ_FMT]		# format for printing row numbers
char	rn_name[SZ_ROW_HDR]	# row number header:  "# row"
char	rn_units[SZ_ROW_HDR]	# "#    " printed in the line for units
char	percent			# '%'
int	nrows			# total number of rows in table
int	rn_width		# width needed for printing row number
int	lcp			# column number of leftmost column on page
int	ncp			# number of columns on current page
int	rownum			# row number
int	linenum			# number of lines of data printed
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
bool	done			# flag for terminating while loop on rows
int	decode_ranges(), get_next_number(), tbpsta()

begin
	# Allocate space for format strings for printing column names and
	# values.  Allocate arrays for pointers to column descriptors and
	# the ascii width of each column.  Allow enough space to print all
	# columns on one page.
	call smark (sp)
	call salloc (cn_fmt, (SZ_FMT+1)*nprint, TY_CHAR)
	call salloc (colptr, nprint, TY_POINTER)
	call salloc (lenfmt, nprint, TY_POINTER)

	nrows = tbpsta (tp, TBL_NROWS)

	if (decode_ranges (range_string, ranges, MAX_RANGES, nvalues) != OK)
	    call error (1, "bad range of row numbers")

	# rn_name, rn_units, rn_fmt and rn_width must be consistent.
	if (showrow) {
	    # These two strings are printed above the row number
	    # in the line for column names and in the line for column units.
	    call strcpy ("# row", rn_name, SZ_ROW_HDR)
	    call strcpy ("#    ", rn_units, SZ_ROW_HDR)
	    rn_width = SZ_ROW_HDR		# space for printing row number
	} else if (showhdr) {
	    call strcpy ("#", rn_name, SZ_ROW_HDR)
	    call strcpy ("#", rn_units, SZ_ROW_HDR)
	    rn_width = 1			# space for "#"
	} else {
	    rn_name[1] = EOS			# no header printed
	    rn_units[1] = EOS
	    rn_width = 0
	}
	percent = '%'
	call sprintf (rn_fmt, SZ_FMT, "%c%dd")	# --> %5d
	    call pargc (percent)
	    call pargi (SZ_ROW_HDR)

	lcp = 1					# initialize
	while (lcp <= nprint) {			# do for each page

	    # Get column pointers for current page
	    call get_page (tp, cptr, lcp, nprint, pagewidth-rn_width,
			align, showunits, Memi[colptr], Memi[lenfmt], ncp)

	    # Fill array of print formats.
	    call tpr_pfmt (Memi[colptr], Memi[lenfmt], align, Memc[cn_fmt], ncp)

	    # Print a form feed if this is not the first page and the user
	    # has requested page breaks.
	    if (lcp > 1) {
		if (pagelength > 0)
		    call printf ("\f\n")
		else
		    call printf ("\n")
	    }
	    # Print column names.
	    if (showhdr)
		call tpr_cnames_pr (Memi[colptr], Memc[cn_fmt], ncp,
			showunits, rn_name, rn_units)

	    # Print each row that is to be printed.
	    linenum = 0				# initialize line counter
	    rownum = 0				# initialize get_next_number
	    stat = get_next_number (ranges, rownum)	# get first row number
	    done = (stat == EOF) || (rownum > nrows)

	    while ( !done ) {

		# Print values in current row.
		call prt_row (tp, Memi[colptr], Memc[cn_fmt], ncp,
			linenum, pagelength, s_cp, lgroup,
			rn_fmt, rn_name, rn_units,
			rownum, showrow, orig_row, showhdr, showunits)

		# Get next row number.
		stat = get_next_number (ranges, rownum)
		done = (stat == EOF) || (rownum > nrows)
	    }

	    lcp = lcp + ncp		# next set of columns to be printed
	}
	call sfree (sp)
end

# tpr_break -- print a page break
# This routine prints a form feed ('\f') if appropriate.  If pagelength
# is zero we're not printing page breaks anyway, and if linenum is zero
# we've already printed the header, so nothing is done.  Otherwise,
# if linenum is zero mod pagelength print a page break, and if showhdr
# print the header.

procedure tpr_break (linenum, pagelength, showhdr, showunits,
		colptr, cn_fmt, ncp, rn_name, rn_units)

int	linenum			# io: number of lines of data printed
int	pagelength		# i: number of data lines per page
bool	showhdr			# i: print column names?
bool	showunits		# i: also print column units?
pointer colptr[ARB]		# i: array of column pointers
char	cn_fmt[SZ_FMT,ARB]	# i: array of print formats
int	ncp			# i: number of columns on current page
char	rn_name[ARB]		# i: column header for row number
char	rn_units[ARB]		# i: printed below rn_name
#--

begin
	if (pagelength > 0) {
	    if (linenum > 0) {
		if (mod (linenum, pagelength) == 0) {
		    call printf ("\f\n")
		    if (showhdr)	# print column names
			call tpr_cnames_pr (colptr, cn_fmt, ncp,
				showunits, rn_name, rn_units)
		}
	    }
	}
end


# get_page -- get columns for page
# This procedure determines which columns will fit on the current page.
# Each column is assumed to begin with a column separator which is a
# single space.

procedure get_page (tp, cptr, lcp, nprint, pagewidth, align,
		showunits, colptr, lenfmt, ncp)

pointer tp			# i: pointer to table descriptor
pointer cptr[nprint]		# i: array of pointers to all column descr
int	lcp			# i: column number of leftmost column on page
int	nprint			# i: number of columns to print
int	pagewidth		# i: page width available for writing
bool	align			# i: override print fmt to align col & name?
bool	showunits		# i: also print column units?
pointer colptr[ARB]		# o: pointers for columns on current page
int	lenfmt[ARB]		# o: length of print format for each col
int	ncp			# o: number of columns on current page
#--
char	colname[SZ_COLNAME]	# column name for error message
int	lentotal		# sum of lenfmt plus a space for each column
int	nextc			# column counter
int	tpr_lenfmt()

begin
	# Assume we can print at least one column, truncated if necessary.
	ncp = 1
	colptr[1] = cptr[lcp]
	# Get width for printing column.
	lenfmt[1] = tpr_lenfmt (colptr[1], align, showunits)
	if (lenfmt[1] > pagewidth-1) {
	    call tbcigt (colptr[1], TBL_COL_NAME, colname, SZ_COLNAME)
	    call eprintf ("caution:  column %s will be truncated\n")
		call pargstr (colname)
	    lenfmt[1] = pagewidth-1
	}

	lentotal = lenfmt[1] + 1		# add one for leading space

	# The loop continuation conditions are:
	#	the columns fit on the page,
	#	there are still columns that have not been included.
	while (lentotal < pagewidth && lcp+ncp-1 < nprint) {
	    nextc = ncp + 1
	    colptr[nextc] = cptr[lcp+nextc-1]
	    # get lenfmt
	    lenfmt[nextc] = tpr_lenfmt (colptr[nextc], align, showunits)
	    lentotal = lentotal + lenfmt[nextc] + 1	# one for leading space
	    if (lentotal <= pagewidth)
		ncp = nextc			# = ncp + 1
	}
end



# tpr_lenfmt -- get length of print format
# This function returns the length of the print format for a column.
# If align is true then the column name will be gotten, and the length
# of the print format that is returned will be at least as large as
# the length of the column name.  The length will also be at least five,
# since that is the length of the word INDEF.

int procedure tpr_lenfmt (cptr, align, showunits)

pointer cptr			# i: pointer to column descriptor
bool	align			# i: true ==> may increase length of print fmt
bool	showunits		# i: also print column units?
#--
char	colname[SZ_COLNAME]	# name of column
char	colunits[SZ_COLUNITS]	# column units
int	lenfmt			# length of print format
int	len_name		# length of column name
int	len_units		# length of column units
int	tbcigi(), strlen()

begin
	lenfmt = tbcigi (cptr, TBL_COL_FMTLEN)

	if ( align ) {

	    call tbcigt (cptr, TBL_COL_NAME, colname, SZ_COLNAME)
	    len_name = strlen (colname)

	    # Length >= length of column name or the word "INDEF".
	    lenfmt = max (lenfmt, len_name, 5)

	    if (showunits) {
		call tbcigt (cptr, TBL_COL_UNITS, colunits, SZ_COLUNITS)
		len_units = strlen (colunits)
		lenfmt = max (lenfmt, len_units)
	    }
	}
	return (lenfmt)
end



# tpr_pfmt -- Get print formats
# This procedure fills an array with print formats of the form %ws.
# These can be used for printing the column names, units and data values.

procedure tpr_pfmt (colptr, lenfmt, align, cn_fmt, ncp)

pointer colptr[ncp]		# i: array of column pointers
int	lenfmt[ncp]		# i: array of lengths of print formats
bool	align			# i: override print fmt to align col & name?
char	cn_fmt[SZ_FMT,ncp]	# o: array of print formats
int	ncp			# i: number of columns on current page
#--
char	fmt[SZ_COLFMT]		# unmodified print format as gotten from table
int	cn			# loop index for column number

begin
	do cn = 1, ncp {			# do for each column on page

	    # Get print format for current column.
	    call tbcigt (colptr[cn], TBL_COL_FMT, fmt, SZ_COLFMT)

	    cn_fmt[1,cn] = '%'
	    if (fmt[2] == '-') {		# left justification
		if (cn == ncp) {
		    call sprintf (cn_fmt[2,cn], SZ_FMT-1, "-s")
		} else {
		    call sprintf (cn_fmt[2,cn], SZ_FMT-1, "-%ds")
			call pargi (lenfmt[cn])
		}
	    } else {				# right justification
		call sprintf (cn_fmt[2,cn], SZ_FMT-1, "%ds")
		    call pargi (lenfmt[cn])
	    }
	}
end


# tpr_cnames_pr -- Print column names
# This procedure prints the column names and units and an extra blank line.
# A comment character ("#") is printed at the beginning of the line; this
# is new as of 1995 Feb 16.

procedure tpr_cnames_pr (colptr, cn_fmt, ncp,
		showunits, rn_name, rn_units)

pointer colptr[ncp]		# i: array of column pointers
char	cn_fmt[SZ_FMT,ncp]	# i: array of print formats
int	ncp			# i: number of columns on current page
bool	showunits		# i: also print column units?
char	rn_name[ARB]		# i: column header for row number
char	rn_units[ARB]		# i: printed below rn_name
#--
char	colname[SZ_COLNAME]	# column name
char	colunits[SZ_COLUNITS]	# column units
int	cn			# loop index for column number

begin
	call printf (rn_name)		# "# row" or "#"

	do cn = 1, ncp {		# do for each column on page
	    call printf (" ")
	    call tbcigt (colptr[cn], TBL_COL_NAME, colname, SZ_COLNAME)
	    call printf (cn_fmt[1,cn])
		call pargstr (colname)
	}
	call printf ("\n")

	# Also print column units?
	if (showunits) {
	    call printf (rn_units)	# "#    " or "#"
	    do cn = 1, ncp {
		call printf (" ")
		call tbcigt (colptr[cn], TBL_COL_UNITS, colunits, SZ_COLUNITS)
		call printf (cn_fmt[1,cn])
		    call pargstr (colunits)
	    }
	    call printf ("\n")
	}

	call printf ("\n")
end


# prt_row -- print a row
# This procedure prints the contents of one row.

procedure prt_row (tp, colptr, cn_fmt, ncp,
		linenum, pagelength, s_cp, lgroup,
		rn_fmt, rn_name, rn_units,
		rownum, showrow, orig_row, showhdr, showunits)

pointer tp			# i: pointer to table descriptor
pointer colptr[ncp]		# i: array of pointers to column descriptors
char	cn_fmt[SZ_FMT,ncp]	# i: array of print formats
int	ncp			# i: number of columns on current page
int	linenum			# io: number of lines of data printed
int	pagelength		# i: number of data lines per page
pointer s_cp			# i: pointer to column to control spacing
int	lgroup			# i: print blank line after this many lines
char	rn_fmt[ARB]		# i: format for printing row number
char	rn_name[ARB]		# i: row number header:  "# row"
char	rn_units[ARB]		# i: "#    " printed in the line for units
int	rownum			# i: row number
bool	showrow			# i: print row number?
bool	orig_row		# i: show row number from underlying table?
bool	showhdr			# i: was a header printed?
bool	showunits		# i: print column units?
#--
pointer sp
pointer cbuf			# scratch for character-string buffer
pointer nelem			# array length for each column
int	max_nelem		# maximum of array lengths
bool	has_arrays		# true if any column contains arrays
bool	has_scalars		# true if not all columns contain arrays
int	cn			# loop index for column number
int	element			# loop index for array element number
int	underlying_row		# row number in underlying table
int	ip			# first non-blank character in cbuf
int	s_flag			# YES if we should print a line for spacing
int	tbcigi(), tbagtt()
errchk	tbsirow

begin
	call smark (sp)
	call salloc (cbuf, SZ_LINE, TY_CHAR)
	call salloc (nelem, ncp, TY_INT)

	# Get the array length for each column.
	max_nelem = 1			# initial values
	if (showrow)
	    has_scalars = true		# row number is a scalar
	else
	    has_scalars = false

	do cn = 1, ncp {
	    Memi[nelem+cn-1] = tbcigi (colptr[cn], TBL_COL_LENDATA)
	    if (Memi[nelem+cn-1] > max_nelem)
		max_nelem = Memi[nelem+cn-1]
	    if (Memi[nelem+cn-1] == 1)
		has_scalars = true
	}
	has_arrays = (max_nelem > 1)

	# If all columns contain arrays, print a blank line as a separator.
	if (!has_scalars && rownum > 1)
	    call printf ("\n")

	# Loop over the number of elements in the longest array.
	do element = 1, max_nelem {

	    # Print a page break if appropriate.
	    call tpr_break (linenum, pagelength, showhdr, showunits,
			colptr, cn_fmt, ncp, rn_name, rn_units)

	    # Print a blank line if the value in the column has changed
	    # or if a group of lgroup lines has been printed.
	    call tpr_space (tp, s_cp, lgroup,
			rownum, element, max_nelem, pagelength, linenum, s_flag)
	    if (s_flag == YES) {
		call printf ("\n")
		linenum = linenum + 1
		# Check whether we should also print a page break.
		call tpr_break (linenum, pagelength, showhdr, showunits,
			colptr, cn_fmt, ncp, rn_name, rn_units)
	    }

	    if (showrow) {
		if (element == 1) {
		    if (orig_row) {
			call tbsirow (tp, rownum, underlying_row)
			call printf (rn_fmt)
			    call pargi (underlying_row)
		    } else {
			call printf (rn_fmt)
			    call pargi (rownum)
		    }
		} else {
		    call printf ("     ")		# SZ_ROW_HDR blanks
		}
	    } else if (showhdr) {
		# Corresponds to the "#" at the beginning of header lines.
		call printf (" ")
	    }

	    do cn = 1, ncp {

		# Even if no row number is printed, start with a space.
		call printf (" ")		# space between columns

		# Does the current column contain arrays?
		if (Memi[nelem+cn-1] > 1) {

		    if (element <= Memi[nelem+cn-1]) {
			if (tbagtt (tp, colptr[cn], rownum,
				Memc[cbuf], SZ_LINE, element, 1) < 1)
			    call error (1, "can't read array element")
			call tpr_noblank (Memc[cbuf], ip)
		    } else {
			Memc[cbuf] = EOS
			ip = 1
		    }
		    call printf (cn_fmt[1,cn])
			call pargstr (Memc[cbuf+ip-1])

		} else if (element == 1) {

		    # This is a scalar column.
		    call tbegtt (tp, colptr[cn], rownum, Memc[cbuf], SZ_LINE)
		    call tpr_noblank (Memc[cbuf], ip)
		    call printf (cn_fmt[1,cn])
			call pargstr (Memc[cbuf+ip-1])

		} else {

		    # Print a blank field for a scalar column to match array
		    # element > 1 for array column(s).
		    call printf (cn_fmt[1,cn])
			call pargstr ("")
		}
	    }
	    call printf ("\n")
	    linenum = linenum + 1
	}
	call sfree (sp)
end

procedure tpr_noblank (buf, ip)

char	buf[ARB]	# io: input string (trailing blanks will be truncted)
int	ip		# o: first non-blank character in buf
#--
int	strlen()

begin
	ip = strlen (buf)
	while (ip >= 1 && IS_WHITE(buf[ip])) {	# trim trailing blanks
	    buf[ip] = EOS
	    ip = ip - 1
	}
	ip = 1
	while (IS_WHITE(buf[ip]))		# trim leading blanks
	    ip = ip + 1
end

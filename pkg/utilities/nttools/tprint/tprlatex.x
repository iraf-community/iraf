include <tbset.h>
include "tprint.h"

# tpr_latex_pr -- print contents of table
# This version prints the table data in a form suitable for input to
# TeX or LaTeX.  The corresponding procedure that prints in plain text
# format is tpr_plain_pr.
#
# Phil Hodge,  7-Oct-1987  Subroutine created
# Phil Hodge, 12-Feb-1988  Call tpr_pfmt_l instead of tpr_pfmt
# Phil Hodge, 30-Mar-1988  Use a column to control spacing of printout.
# Phil Hodge,  6-Jan-1989  tpr_break_l for new page, also after tpr_cnames_pr.
# Phil Hodge,  2-Apr-1993  In prt_row_l, include short datatype.
# Phil Hodge,  5-Jul-1993  Include option to print column units.
# Phil Hodge, 26-Mar-1998  Add orig_row to calling sequence, use in prt_row_l;
#			in tpr_break_l, the calling sequence of tpr_end_tbl
#			had an extra argument.
# Phil Hodge,  9-Aug-1999  Change the calling sequence of tpr_space.

procedure tpr_latex_pr (tp, colptr, ncp, s_cp, lgroup,
		range_string, pagewidth, pagelength,
		showrow, orig_row, showhdr, showunits, prt_option)

pointer tp			# i: pointer to table descriptor
pointer colptr[ncp]		# i: array of pointers to column descriptors
int	ncp			# i: number of columns to print
pointer s_cp			# i: pointer to column to control spacing
int	lgroup			# i: print a blank line after this many lines
char	range_string[ARB]	# i: string which gives ranges of row numbers
int	pagewidth		# i: page width
int	pagelength		# i: number of data lines per page
bool	showrow			# i: true if row number is to be printed
bool	orig_row		# i: show row number from underlying table?
bool	showhdr			# i: print column names, etc?
bool	showunits		# i: print column units?
char	prt_option[ARB]		# i: "latex" or "tex"
#--
pointer sp
pointer data_fmt		# print formats for column data values
pointer j_flag			# left or right justification
char	rn_fmt[SZ_FMT]		# format for printing row numbers
char	rn_name[SZ_ROW_HDR]	# row number header:  "(row)"
char	percent			# a percent sign (comment for TeX)
int	nrows			# total number of rows in table
int	element			# loop index for array element number
int	max_nelem		# max number of elements
int	rn_width		# width needed for printing row number
int	lenfmt[MAXCOLS]		# lengths of print fmt for cols on current page
int	rownum			# row number
int	linenum			# number of lines of data printed
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
int	k			# loop index
int	s_flag			# YES if we should add a line for spacing
bool	done			# flag for terminating while loop on rows
int	decode_ranges(), get_next_number(), tbpsta(), tbcigi()

begin
	percent = '%'

	# Allocate space for format strings for printing column names
	# and values.
	call smark (sp)
	call salloc (data_fmt, (SZ_FMT+1)*ncp, TY_CHAR)
	call salloc (j_flag, ncp, TY_INT)

	if (ncp > MAXCOLS) {
	    call eprintf ("maximum number of columns is %d\n")
		call pargi (MAXCOLS)
	    call error (1, "")
	}

	nrows = tbpsta (tp, TBL_NROWS)

	if (decode_ranges (range_string, ranges, MAX_RANGES, nvalues) != OK)
	    call error (1, "bad range of row numbers")

	# These three values (rn_name, rn_fmt, rn_width) must be consistent.
	call strcpy ("(row)", rn_name, SHORT_STRING)
	call strcpy ("%5d", rn_fmt, SZ_FMT)
	if (showrow)
	    rn_width = SZ_ROW_HDR		# space for printing row number
	else
	    rn_width = 0

	# Get length of print format for each column.
	do k = 1, ncp
	    lenfmt[k] = tbcigi (colptr[k], TBL_COL_FMTLEN)

	# Fill array of print formats for column names and for data.
	call tpr_pfmt_l (colptr, lenfmt, Memc[data_fmt],
			Memi[j_flag], ncp)

	if (showhdr) {
	    # Print the \begin{document} string.
	    call tpr_beg_doc (prt_option)

	    # Print the default \def or \newcommand for column separators
	    # and \eol, and print the begin-table string and column names.
	    call tpr_def (prt_option, ncp, showrow)
	    call tpr_begin_tbl (prt_option, Memi[j_flag], ncp, showrow)
	    call tpr_cnames_pr_l (colptr, ncp, showrow, showunits, rn_name)
	}

	# Print each row that is to be printed.
	linenum = 0				# initialize line counter
	rownum = 0				# initialize get_next_number
	stat = get_next_number (ranges, rownum)	# get first row number
	done = (stat == EOF) || (rownum > nrows)

	element = 1	# not used yet
	max_nelem = 1

	while ( !done ) {

	    # Print a page break if appropriate.
	    call tpr_break_l (linenum, pagelength, showhdr, showunits,
			prt_option, Memi[j_flag],
			colptr, ncp, showrow, rn_name)

	    # Print a blank line if the column value has changed or if
	    # a group of lgroup lines have been printed.
	    call tpr_space (tp, s_cp, lgroup,
			rownum, element, max_nelem, pagelength, linenum, s_flag)
	    if (s_flag == YES) {
		call printf ("\\extline\n")
		linenum = linenum + 1
		# Check whether we should also print a page break.
		call tpr_break_l (linenum, pagelength, showhdr, showunits,
			prt_option, Memi[j_flag],
			colptr, ncp, showrow, rn_name)
	    }

	    # Print % as row separator (for readability); print current row.
	    call printf ("%c\n")
		call pargc (percent)
	    call prt_row_l (tp, colptr, Memc[data_fmt], ncp, rownum,
		lenfmt, pagewidth, showrow, orig_row, rn_fmt, rn_width)
	    linenum = linenum + 1
	    stat = get_next_number (ranges, rownum)
	    done = (stat == EOF) || (rownum > nrows)
	}

	if (showhdr) {
	    # Print end-table string.
	    call tpr_end_tbl (prt_option)
	}
	if (showhdr)
	    # Write \end{document} string.
	    call tpr_end_doc (prt_option)

	call sfree (sp)
end

# tpr_break_l -- print a page break
# This routine prints the "end table" and "begin table" strings, if
# appropriate.  If pagelength is zero we're not printing page breaks
# anyway, and if linenum is zero we've already printed the header, so
# nothing is done.  Otherwise, if linenum is zero mod pagelength
# print a page break, and if showhdr print the column names.

procedure tpr_break_l (linenum, pagelength, showhdr, showunits,
		prt_option, just_flag,
		colptr, ncp, showrow, rn_name)

int	linenum			# io: number of lines of data printed
int	pagelength		# i: number of data lines per page
bool	showhdr			# i: print column names?
bool	showunits		# i: print column units?
char	prt_option[ARB]		# i: "latex" or "tex"
int	just_flag[ARB]		# i: -1, 0, +1 for left, center, right just.
pointer colptr[ARB]		# i: array of column pointers
int	ncp			# i: number of columns to print
bool	showrow			# i: print row number?
char	rn_name[ARB]		# i: column header for row number
#--

begin
	if (pagelength > 0) {
	    if (linenum > 0) {
		if (mod (linenum, pagelength) == 0) {
		    # Print end table, begin table, and column names.
		    call tpr_end_tbl (prt_option)
		    call tpr_begin_tbl (prt_option, just_flag, ncp,
				showrow)
		    if (showhdr)
			call tpr_cnames_pr_l (colptr, ncp,
				showrow, showunits, rn_name)
		}
	    }
	}
end


# tpr_pfmt_l -- Get print formats
# This procedure fills an array with print formats for printing the
# column values.  An array of flags specifying whether each column is
# to be left or right justified is also returned.

procedure tpr_pfmt_l (colptr, lenfmt, data_fmt, just_flag, ncp)

pointer colptr[ncp]		# i: array of column pointers
int	lenfmt[ncp]		# i: array of lengths of print formats
char	data_fmt[SZ_FMT,ncp]	# o: array of print formats for data
int	just_flag[ncp]		# o: -1 or +1 for left, right justification
int	ncp			# i: number of columns to print
#--
int	cn			# loop index for column number

begin
	do cn = 1, ncp {			# do for each column to print

	    call tbcigt (colptr[cn], TBL_COL_FMT, data_fmt[1,cn], SZ_FMT)
	    if (data_fmt[2,cn] == '-')
		just_flag[cn] = -1		# left justification
	    else
		just_flag[cn] = 1		# right justification
	}
end



# tpr_beg_doc -- Print begin-document strings
# This procedure prints strings for LaTeX or for TeX that begin
# a document.  (Nothing is written for TeX.)

procedure tpr_beg_doc (prt_option)

char	prt_option[ARB]		# i: "latex" or "tex"

begin
	if (prt_option[1] == 'l') {		# LaTeX
	    call printf ("\\documentstyle{article}\n")
	    call printf ("\\begin{document}\n")
	}
end



# tpr_end_doc -- Print end-document strings
# This procedure prints strings for LaTeX or for TeX that end
# a document.

procedure tpr_end_doc (prt_option)

char	prt_option[ARB]		# i: "latex" or "tex"

begin
	if (prt_option[1] == 'l')		# LaTeX
	    call printf ("\\end{document}\n")
	else if (prt_option[1] == 't')		# TeX
	    call printf ("\\end\n")
end


# tpr_def -- Print newcommand strings
# This procedure prints strings for LaTeX or for TeX that define
# macros for column separators and for the end-of-line string.
#
# Phil Hodge,  1-Apr-88  \extline added

procedure tpr_def (prt_option, ncp, showrow)

char	prt_option[ARB]		# i: "latex" or "tex"
int	ncp			# i: number of columns to print
bool	showrow			# i: print row number?
#--
int	k			# loop index
char	new_cmd[SHORT_STRING]	# "\newcommand" or "\def"
char	n_str[SHORT_STRING]	# "{\null}"
char	latex_eol[SHORT_STRING]	# "\eol{\\}"
char	tex_eol[SHORT_STRING]	# "\eol{\cr}"

begin
	if (prt_option[1] == 'l')		# LaTeX
	    call strcpy ("\\newcommand", new_cmd, SHORT_STRING)
	else if (prt_option[1] == 't')		# TeX
	    call strcpy ("\\def", new_cmd, SHORT_STRING)
	call strcpy ("{\\null}", n_str, SHORT_STRING)
	call strcpy ("\\eol{\\\\}", latex_eol, SHORT_STRING)
	call strcpy ("\\eol{\\cr}",   tex_eol, SHORT_STRING)

	if (showrow)
	    k = 0
	else
	    k = 1

	# Define either \colzero or \cola, depending on showrow.
	call printf ("%s")
	    call pargstr (new_cmd)
	call tpr_w_colsep (k)
	call printf ("%s\n")
	    call pargstr (n_str)

	# Define the rest of the column-separators, if any.
	k = k + 1
	while (k <= ncp) {
	    call printf ("%s")
		call pargstr (new_cmd)
	    call tpr_w_colsep (k)
	    call printf ("{&}\n")
	    k = k + 1
	}

	# Define \eol.
	call printf ("%s")
	    call pargstr (new_cmd)
	if (prt_option[1] == 'l') {			# LaTeX
	    call printf ("%s\n")
		call pargstr (latex_eol)
	} else if (prt_option[1] == 't') {		# TeX
	    call printf ("%s\n")
		call pargstr (tex_eol)
	}

	# Define \extline for writing blank lines.
	call printf ("%s\\extline{")
	    call pargstr (new_cmd)
	do k = 1, ncp-1
	    call printf ("&")
	if (showrow)
	    call printf ("&")
	call printf ("\\eol}\n")

	call printf ("\n")
end



# tpr_begin_tbl -- Print begin-table string
# This procedure prints a begin-table string for LaTeX or for TeX.

procedure tpr_begin_tbl (prt_option, just_flag, ncp, showrow)

char	prt_option[ARB]		# i: "latex" or "tex"
int	just_flag[ARB]		# i: -1, 0, +1 for left, center, right just.
int	ncp			# i: number of columns to print
bool	showrow			# i: print row number?
#--
int	k			# loop index
char	tex_cr[SHORT_STRING]	# "\cr"

begin
	if (prt_option[1] == 'l') {		# LaTeX

	    call printf ("\\begin{tabular}{")
	    if (showrow)
		call printf ("r")		# right justify row number
	    do k = 1, ncp {
		if (just_flag[k] == -1)
		    call printf ("l")		# left justify
		else if (just_flag[k] == 1)
		    call printf ("r")		# right justify
		else
		    call printf ("c")		# center
	    }
	    call printf ("}\n")

	} else if (prt_option[1] == 't') {	# TeX

	    call strcpy ("\\cr", tex_cr, SHORT_STRING)

	    if (showrow) {
		call printf ("\\halign{\\hfil#")	# row number
		call printf ("\n&\\quad")
	    } else {
		call printf ("\\halign{")
	    }

	    # First column.
	    if (just_flag[1] == -1)
		call printf ("#\\hfil")			# left
	    else if (just_flag[1] == 1)
		call printf ("\\hfil#")			# right
	    else
		call printf ("\\hfil#\\hfil")		# center

	    do k = 2, ncp {
		if (just_flag[k] == -1)
		    call printf ("\n&\\quad#\\hfil")
		else if (just_flag[k] == 1)
		    call printf ("\n&\\quad\\hfil#")
		else
		    call printf ("\n&\\quad\\hfil#\\hfil")
	    }
	    call printf ("%s\n\n")		# can't use \eol here
		call pargstr (tex_cr)
	}
end


# tpr_end_tbl -- Print end-table string
# This procedure prints an end-table string for LaTeX (or TeX).

procedure tpr_end_tbl (prt_option)

char	prt_option[ARB]		# i: "latex" or "tex"
#--

begin
	if (prt_option[1] == 'l')		# LaTeX
	    call printf ("\\end{tabular}\n\n")
	else if (prt_option[1] == 't')		# TeX
	    call printf ("}\n\n")
end



# tpr_cnames_pr_l -- Print column names
# This procedure prints the column names followed by a blank line.
# (TeX or LaTeX only)

procedure tpr_cnames_pr_l (colptr, ncp, showrow, showunits, rn_name)

pointer colptr[ncp]		# i: array of column pointers
int	ncp			# i: number of columns on current page
bool	showrow			# i: true if row number is to be printed
bool	showunits		# i: print column units?
char	rn_name[ARB]		# i: column header for row number
#--
int	cn			# loop index for column number
char	colname[SZ_COLNAME]	# column name
char	colunits[SZ_COLUNITS]	# column units

begin
	if (showrow) {
	    call tpr_w_colsep (0)
	    call printf (rn_name)
	}
	do cn = 1, ncp {			# do for each column on page
	    call tpr_w_colsep (cn)
	    call tbcigt (colptr[cn], TBL_COL_NAME, colname, SZ_COLNAME)
	    call printf ("%s")			# trim extra blanks
		call pargstr (colname)
	}
	call printf ("\\eol\n")

	# Also print column units?
	if (showunits) {
	    if (showrow) {
		call tpr_w_colsep (0)
	    }
	    do cn = 1, ncp {
		call tpr_w_colsep (cn)
		call tbcigt (colptr[cn], TBL_COL_UNITS, colunits, SZ_COLUNITS)
		call printf ("%s")
		    call pargstr (colunits)
	    }
	    call printf ("\\eol\n")
	}
	call printf ("\\extline\n")
end


# prt_row_l -- print a row
# This procedure prints the contents of one row.  This LaTeX (or TeX)
# version differs from the plain-print version in the following ways:
# character-string values are printed using %s so that extra blanks
# will not be printed, the column-separators may differ from one column
# to the next, and an end-of-line string is printed.

procedure prt_row_l (tp, colptr, data_fmt, ncp, rownum,
		lenfmt, pagewidth, showrow, orig_row, rn_fmt, rn_width)

pointer tp			# i: pointer to table descriptor
pointer colptr[ncp]		# i: array of pointers to column descriptors
char	data_fmt[SZ_FMT,ncp]	# i: print format for each column
int	ncp			# i: number of columns on current page
int	rownum			# i: row number
int	lenfmt[ncp]		# i: array of lengths of print formats
int	pagewidth		# i: page width
bool	showrow			# i: print row number?
bool	orig_row		# i: show row number from underlying table?
char	rn_fmt[ARB]		# i: format for printing row number
int	rn_width		# i: space for printing row number
#--
pointer sp
pointer cbuf			# scratch for character-string buffer
double	dbuf			# buffer for double-precision elements
real	rbuf			# buffer for single-precision elements
int	ibuf			# buffer for integer elements
short	sbuf
bool	bbuf			# buffer for boolean elements
int	cn			# loop index for column number
int	datatype		# data type of column
int	lentotal		# for determining when to print \n
int	colsep_width		# space needed to print column-separator string
int	underlying_row		# row number in underlying table
int	tbcigi()
errchk	tbsirow

begin
	call smark (sp)
	call salloc (cbuf, SZ_LINE, TY_CHAR)

	colsep_width = 6			# e.g. "\cola "

	if (showrow) {
	    call tpr_w_colsep (0)
	    if (orig_row) {
		call tbsirow (tp, rownum, underlying_row)
		call printf (rn_fmt)
		    call pargi (underlying_row)
	    } else {
		call printf (rn_fmt)
		    call pargi (rownum)		# write row number
	    }
	    lentotal = colsep_width + rn_width
	} else {
	    lentotal = 0
	}

	do cn = 1, ncp {

	    if (lentotal + lenfmt[cn] > pagewidth) {
		call printf ("\n")
		lentotal = 0
	    }
	    call tpr_w_colsep (cn)		# write column-separator string

	    datatype = tbcigi (colptr[cn], TBL_COL_DATATYPE)
	    switch (datatype) {
	    case (TY_REAL):
		call tbegtr (tp, colptr[cn], rownum, rbuf)
		call printf (data_fmt[1,cn])
		    call pargr (rbuf)
	    case (TY_DOUBLE):
		call tbegtd (tp, colptr[cn], rownum, dbuf)
		call printf (data_fmt[1,cn])
		    call pargd (dbuf)
	    case (TY_INT):
		call tbegti (tp, colptr[cn], rownum, ibuf)
		call printf (data_fmt[1,cn])
		    call pargi (ibuf)
	    case (TY_SHORT):
		call tbegts (tp, colptr[cn], rownum, sbuf)
		call printf (data_fmt[1,cn])
		    call pargs (sbuf)
	    case (TY_BOOL):
		call tbegtb (tp, colptr[cn], rownum, bbuf)
		call printf (data_fmt[1,cn])
		    call pargb (bbuf)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call tbegtt (tp, colptr[cn], rownum, Memc[cbuf], SZ_LINE)
		    call printf ("%s")			# trim blanks
			call pargstr (Memc[cbuf])
		} else {
		    call error (1, "bad data type; table corrupted?")
		}
	    }
	    lentotal = lentotal + colsep_width + lenfmt[cn]
	}
	call printf ("\\eol\n")

	call sfree (sp)
end


# tpr_w_colsep -- Write column-separator string
# This procedure writes a string of the form "\cola ", "\colb ", etc
# for n = 1, 2, etc.  The case n = 0 gives "\colzero " which is used
# for the row number column.

procedure tpr_w_colsep (n)

int	n			# i: column number or zero
#--
string	alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

begin
	if (n <= 0) {
	    call printf ("\\colzero ")
	} else {
	    call printf ("\\col%c ")
		call pargc (alphabet[n])
	}
end

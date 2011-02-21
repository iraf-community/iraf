include <ctype.h>	# for IS_WHITE
include <tbset.h>
include "tprint.h"

define	ALIGN_LEFT	-1
define	ALIGN_CENTER	0	# currently not used
define	ALIGN_RIGHT	1

# This file contains subroutines for printing header keywords and/or
# table data in html table format.
# The high-level subroutines are:
#
#	tpr_html_begin
#	tpr_html_end
#	tpr_html_param		print header keywords
#	tpr_html_pr		print table data
#
# Phil Hodge,  9-Aug-1999  Subroutine created

procedure tpr_html_begin()

begin
	call printf ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n")
	call printf ("<HTML>\n")
end

procedure tpr_html_end()

begin
	call printf ("</HTML>\n")
end

# tpr_html_pr -- print contents of table
# This version prints the table data in html format.

procedure tpr_html_pr (tp, colptr, ncp, s_cp, lgroup,
		range_string, pagelength,
		showrow, orig_row, showhdr, showunits)

pointer tp			# i: pointer to table descriptor
pointer colptr[ncp]		# i: array of pointers to column descriptors
int	ncp			# i: number of columns to print
pointer s_cp			# i: pointer to column to control spacing
int	lgroup			# i: print blank line after this many lines
char	range_string[ARB]	# i: string which gives ranges of row numbers
int	pagelength		# i: number of data lines before printing header
bool	showrow			# i: true if row number is to be printed
bool	orig_row		# i: show row number from underlying table?
bool	showhdr			# i: print column names, etc?
bool	showunits		# i: print column units?
#--
pointer sp
pointer buf			# for a table entry; also for table info
pointer nelem			# array length for each column
pointer align			# array of flags for column alignment
int	max_nelem		# maximum of array lengths
bool	has_arrays		# true if any column contains arrays
bool	has_scalars		# true if not all columns contain arrays
char	rowspan[SZ_FNAME]	# string possibly containing ROWSPAN=max_nelem
int	nspan			# alternate ROWSPAN=nspan, if extra spacing
int	linenum			# number of lines of data printed
int	tmp_linenum		# temporary value for linenum
int	line_on_page		# number of data lines printed on current page
int	nrows			# number of rows in table
int	rownum			# row number
int	row			# row number to be printed
int	cn			# loop index for column number
int	element			# loop index for element within array
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
int	s_flag			# YES if we should add a line for spacing
int	s_nelem			# array size of s_cp column, or one
bool	done			# flag for terminating while loop on rows
int	decode_ranges(), get_next_number()
int	tbpsta(), tbcigi(), tbagtt()
errchk	tbsirow

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (nelem, ncp, TY_INT)
	call salloc (align, ncp, TY_INT)

	# Get info for each column.
	max_nelem = 1			# initial values
	if (showrow)
	    has_scalars = true		# row number is a scalar
	else
	    has_scalars = false

	if (s_cp == NULL)
	    s_nelem = 1
	else
	    s_nelem = tbcigi (s_cp, TBL_COL_LENDATA)

	do cn = 1, ncp {

	    # array length
	    Memi[nelem+cn-1] = tbcigi (colptr[cn], TBL_COL_LENDATA)
	    if (Memi[nelem+cn-1] > max_nelem)
		max_nelem = Memi[nelem+cn-1]
	    if (Memi[nelem+cn-1] == 1)
		has_scalars = true

	    # left or right alignment, depending on print format
	    call tbcigt (colptr[cn], TBL_COL_FMT, Memc[buf], SZ_LINE)
	    if (Memc[buf+1] == '-')		# e.g. %-12s
		Memi[align+cn-1] = ALIGN_LEFT
	    else
		Memi[align+cn-1] = ALIGN_RIGHT
	}
	if (max_nelem > 1) {
	    has_arrays = true
	    nspan = max_nelem			# initial value
	    if (lgroup > 1) {
		nspan = nspan + max_nelem / (lgroup - 1)
		if (max_nelem / (lgroup - 1) * (lgroup - 1) == max_nelem)
		    nspan = nspan - 1		# one entire line will be blank
	    }
	    call sprintf (rowspan, SZ_FNAME, "ROWSPAN=%d")
		call pargi (nspan)
	} else {
	    has_arrays = false
	    rowspan[1] = EOS
	}

	call tbtnam (tp, Memc[buf], SZ_LINE)

	call printf ("\n")
	call printf ("<HEAD><TITLE>tprint of %s</TITLE></HEAD>\n")
	    call pargstr (Memc[buf])
	call printf ("<BODY>\n")
	call printf ("\n")

	call printf ("<TABLE BORDER=2>\n")
	call printf ("<CAPTION ALIGN=TOP>\n")
	call printf ("<B>Table data:  %s</B>\n")
	    call pargstr (Memc[buf])
	call printf ("</CAPTION>\n")
	call printf ("\n")

	nrows = tbpsta (tp, TBL_NROWS)

	if (showhdr) {
	    call tpr_h_header (tp, colptr, ncp, showrow, showunits,
			Memc[buf], SZ_LINE)
	}

	if (decode_ranges (range_string, ranges, MAX_RANGES, nvalues) != OK)
	    call error (1, "bad range of row numbers")

	# Print each row that is to be printed.
	linenum = 0				# initialize line counters
	line_on_page = 0
	rownum = 0				# initialize get_next_number
	stat = get_next_number (ranges, rownum)	# get first row number
	done = (stat == EOF) || (rownum > nrows)

	while ( !done ) {

	    # If we need to insert extra lines for spacing within a column
	    # that contains arrays, find out the total number of lines
	    # we'll need to print.  Then set ROWSPAN to this new value.
	    if (s_nelem > 1) {
		# Count the total number of elements we'll span.
		nspan = max_nelem		# initial value
		tmp_linenum = linenum
		do element = 1, max_nelem {
		    call tpr_h_space (tp, s_cp, s_nelem, lgroup,
				rownum, element, max_nelem, tmp_linenum, nspan)
		    tmp_linenum = tmp_linenum + 1
		}
		# Overwrite original value of rowspan string.
		call sprintf (rowspan, SZ_FNAME, "ROWSPAN=%d")
			call pargi (nspan)
	    }

	    # If all columns contain arrays, print a blank line.
	    if (!has_scalars && rownum > 1)
		call tpr_h_blank_line (ncp, showrow)

	    # Loop over the number of elements in the longest array.
	    do element = 1, max_nelem {

		# Check whether we should print a blank line.
		# (Set pagelength to zero for this call; in tpr_space,
		# blocking into groups of lines is reset at the top of
		# each page, but we don't do that with html output.)
		call tpr_space (tp, s_cp, lgroup,
			rownum, element, max_nelem, 0, linenum, s_flag)

		if (s_flag == YES) {
		    # Print a blank line.
		    if (has_arrays && element > 1) {
			# Print a blank field for each array column.
			call printf ("    <TR>\n")
			do cn = 1, ncp {
			    if (Memi[nelem+cn-1] > 1)
				call printf ("        <TD>&nbsp;</TD>\n")
			}
			call printf ("    </TR>\n")
		    } else {
			call tpr_h_blank_line (ncp, showrow)
		    }
		    linenum = linenum + 1
		    line_on_page = line_on_page + 1
		}

		# Print column names again, if appropriate.
		if (showhdr && element == 1 && pagelength > 0) {
		    if (line_on_page >= pagelength) {
			call tpr_h_header (tp, colptr, ncp, showrow, showunits,
				Memc[buf], SZ_LINE)
			line_on_page = 0
		    }
		}

		call printf ("    <TR>\n")

		if (element == 1 && showrow) {
		    if (orig_row)
			call tbsirow (tp, rownum, row)
		    else
			row = rownum
		    if (has_arrays) {
			call printf ("        <TD ALIGN=RIGHT %s>%d</TD>\n")
			    call pargstr (rowspan)
			    call pargi (row)
		    } else {
			call printf ("        <TD ALIGN=RIGHT>%d</TD>\n")
			    call pargi (row)
		    }
		}

		# Print each column.
		do cn = 1, ncp {

		    # Does the current column contain arrays?
		    if (Memi[nelem+cn-1] > 1) {

			if (element <= Memi[nelem+cn-1]) {
			    if (tbagtt (tp, colptr[cn], rownum,
				Memc[buf], SZ_LINE, element, 1) < 1)
				call error (1, "can't read array element")
			    call tpr_cell (Memc[buf], Memi[align+cn-1],
					false, "")
			} else {
			    call printf ("        <TD>&nbsp;</TD>\n")
			}

		    } else if (element == 1) {

			call tbegtt (tp, colptr[cn], rownum,
				Memc[buf], SZ_LINE)
			if (has_arrays) {
			    call tpr_cell (Memc[buf], Memi[align+cn-1],
					true, rowspan)
			} else {
			    call tpr_cell (Memc[buf], Memi[align+cn-1],
					false, "")
			}
		    }
		}

		call printf ("    </TR>\n")
		linenum = linenum + 1
		line_on_page = line_on_page + 1
	    }

	    stat = get_next_number (ranges, rownum)
	    done = (stat == EOF) || (rownum > nrows)
	}

	# Print column names at the end of the document, if appropriate.
	if (showhdr && pagelength > 0) {
	    if (line_on_page >= pagelength) {
		call tpr_h_header (tp, colptr, ncp, showrow, showunits,
				Memc[buf], SZ_LINE)
	    }
	}

	call printf ("</TABLE>\n")
	call printf ("</BODY>\n")
	call flush (STDOUT)

	call sfree (sp)
end

procedure tpr_h_blank_line (ncp, showrow)

int	ncp		# i: number of columns to print
bool	showrow		# i: true if we also print row numbers
#--
int	nspan		# number of columns to span

begin
	if (showrow)
	   nspan = ncp + 1
	else
	   nspan = ncp

	call printf ("    <TR>\n")
	call printf ("        <TD COLSPAN=%d>&nbsp;</TD>\n")
	    call pargi (nspan)
	call printf ("    </TR>\n")
end

# This is a simplified version of tpr_space which just increments the count
# of the number of elements to be spanned in a ROWSPAN tag.
#
# We can't just call tpr_space to do this, because each time it examines
# a row, it saves the current value as 'previous'.  We need this routine
# in order to have a separate 'previous' variable.
#
# We don't need to increment linenum if nspan is incremented, because we
# only use this routine for spacing in arrays, and it's the element number
# that we care about for that case.

procedure tpr_h_space (tp, s_cp, s_nelem, lgroup,
		rownum, element, max_nelem, linenum, nspan)

pointer tp			# i: pointer to table descriptor
pointer s_cp			# i: pointer to column to control spacing
int	s_nelem			# i: array size of s_cp column
int	lgroup			# i: print blank after this many lines
int	rownum			# i: number of current row
int	element			# i: array element number
int	max_nelem		# i: max value for element
int	linenum			# i: number of lines that have been printed
int	nspan			# io: incremented if we should increment ROWSPAN
#--
pointer sp
pointer current			# scratch for value of column in current row
int	lpage			# number of line we would print
int	s_flag			# YES if we would print a line for spacing
char	previous[SZ_LINE]	# value of column in previous row
bool	do_compare		# true if we should compare column values
bool	strne()
int	junk, tbagtt()
errchk	tbegtt, tbagtt

begin
	s_flag = NO				# may be changed later

	# linenum is the number of lines that have already been printed.
	# lpage is the current line number.
	lpage = linenum + 1

	# If this is the first line, get the current value of the column
	# and save it as "previous".  That's all.
	if (lpage == 1) {
	    junk = tbagtt (tp, s_cp, rownum, previous, SZ_LINE, element, 1)
	    return
	}

	# Have we printed a group of lines?  If so, set the flag to indicate
	# that we should print a blank line.
	if (lgroup > 1) {
	    if (max_nelem > 1) {
		if (lpage > 1) {
		    if (lgroup == 2)
			s_flag = YES
		    else if (mod (element, lgroup-1) == 1)
			s_flag = YES
		}
	    } else if (mod (lpage, lgroup) == 0) {
		s_flag = YES
	    }
	}

	# Check the value in the column.
	if (element <= s_nelem) {
	    if (s_flag == YES) {
		# Since we already know we need to print a space, we don't
		# have to compare current and previous values, but we still
		# must save current value as "previous".
		junk = tbagtt (tp, s_cp, rownum, previous, SZ_LINE,
				element, 1)
	    } else {
		# Get current value, and compare it with previous value.
		call smark (sp)
		call salloc (current, SZ_LINE, TY_CHAR)
		do_compare = true			# may be reset
		junk = tbagtt (tp, s_cp, rownum, Memc[current], SZ_LINE,
				element, 1)
		if (do_compare && strne (Memc[current], previous)) {
		    # Set flag; save current value as previous value.
		    s_flag = YES
		    call strcpy (Memc[current], previous, SZ_LINE)
		}
		call sfree (sp)
	    }
	}

	# If we should print a line before the first element of an array,
	# we'll print a full line, rather than adding one to ROWSPAN; this
	# is why we include the test on element.
	if (s_flag == YES && element > 1)
	    nspan = nspan + 1
end

# This routine trims trailing blanks from the input buffer (in-place),
# and it sets ip to the index of the first non-blank character.  If
# the entire field is blank, the string "&nbsp;" is assigned to the
# input buffer.

procedure tpr_deblank (buf, ip)

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
	if (buf[ip] == EOS) {
	    call strcpy ("&nbsp;", buf, SZ_LINE)
	    ip = 1
	}
end

# This routine prints one table element.

procedure tpr_cell (buf, align, print_rowspan, rowspan)

char	buf[ARB]	# io: input string (trailing blanks will be truncted)
int	align		# i: ALIGN_LEFT or ALIGN_RIGHT
bool	print_rowspan	# i: true --> print rowspan string
char	rowspan[ARB]	# i: ROWSPAN=<n>
#--
int	ip		# first non-blank character in buf

begin
	call tpr_deblank (buf, ip)

	if (print_rowspan) {

	    if (align == ALIGN_LEFT) {
		call printf ("        <TD ALIGN=LEFT %s>%s</TD>\n")
		    call pargstr (rowspan)
		    call pargstr (buf[ip])
	    } else {
		call printf ("        <TD ALIGN=RIGHT %s>%s</TD>\n")
		    call pargstr (rowspan)
		    call pargstr (buf[ip])
	    }

	} else {

	    if (align == ALIGN_LEFT) {
		call printf ("        <TD ALIGN=LEFT>%s</TD>\n")
		    call pargstr (buf[ip])
	    } else {
		call printf ("        <TD ALIGN=RIGHT>%s</TD>\n")
		    call pargstr (buf[ip])
	    }
	}
end

procedure tpr_h_header (tp, colptr, ncp, showrow, showunits, buf, maxch)

pointer tp			# i: pointer to table descriptor
pointer colptr[ncp]		# i: array of pointers to column descriptors
int	ncp			# i: number of columns to print
bool	showrow			# i: true if row number is to be printed
bool	showunits		# i: print column units?
char	buf[ARB]		# o: scratch space
int	maxch			# i: size of buf
#--
int	cn			# loop index for column number

begin
	call printf ("    <TR>\n")

	if (showrow)
	    call printf ("        <TH ALIGN=RIGHT>(row)</TH>\n")

	do cn = 1, ncp {
	    call tbcigt (colptr[cn], TBL_COL_NAME, buf, SZ_LINE)
	    call printf ("        <TH>%s</TH>\n")
		call pargstr (buf)
	}
	call printf ("    </TR>\n")

	if (showunits) {
	    call printf ("    <TR>\n")
	    if (showrow)
		call printf ("        <TH>&nbsp;</TH>\n")
	    do cn = 1, ncp {
		call tbcigt (colptr[cn], TBL_COL_UNITS, buf, SZ_LINE)
		if (buf[1] == EOS) {
		    call printf ("        <TH>&nbsp;</TH>\n")
		} else {
		    call printf ("        <TH>%s</TH>\n")
			call pargstr (buf)
		}
	    }
	    call printf ("    </TR>\n")
	}
end

# This routine prints all the header keywords for a table.

procedure tpr_html_param (tp)

pointer tp			# i: pointer to table descriptor
#--
pointer sp
pointer buf			# for a keyword value
pointer comment			# keyword comment
int	npar			# number of header parameters
char	keyword[SZ_KEYWORD]	# keyword name
int	n			# loop index for keyword number
int	dtype			# returned by tbhgnp and ignored
int	tbpsta()

begin
	npar = tbpsta (tp, TBL_NPAR)
	if (npar <= 0)
	    return

	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	call tbtnam (tp, Memc[buf], SZ_LINE)

	call printf ("\n")
	call printf ("<HEAD><TITLE>tprint of %s</TITLE></HEAD>\n")
	    call pargstr (Memc[buf])
	call printf ("<BODY>\n")
	call printf ("\n")

	call printf ("<TABLE BORDER=2>\n")
	call printf ("<CAPTION ALIGN=TOP>\n")
	call printf ("<B>Table keywords:  %s</B>\n")
	    call pargstr (Memc[buf])
	call printf ("</CAPTION>\n")
	call printf ("\n")

	call printf ("    <TR>\n")
	call printf ("        <TH>keyword</TH>\n")
	call printf ("        <TH>value</TH>\n")
	call printf ("        <TH>comment</TH>\n")
	call printf ("    </TR>\n")

	do n = 1, npar {

	    call printf ("    <TR>\n")

	    # Get the Nth header parameter and comment.
	    call tbhgnp (tp, n, keyword, dtype, Memc[buf])
	    call tbhgcm (tp, keyword, Memc[comment], SZ_FNAME)

	    if (keyword[1] == EOS) {
		call printf ("        <TD>&nbsp;</TD>\n")
	    } else {
		call printf ("        <TD>%s</TD>\n")
		    call pargstr (keyword)
	    }

	    if (Memc[buf] == EOS) {
		call printf ("        <TD>&nbsp;</TD>\n")
	    } else {
		call printf ("        <TD>%s</TD>\n")
		    call pargstr (Memc[buf])
	    }

	    if (Memc[comment] == EOS) {
		call printf ("        <TD>&nbsp;</TD>\n")
	    } else {
		call printf ("        <TD>%s</TD>\n")
		    call pargstr (Memc[comment])
	    }

	    call printf ("    </TR>\n")
	}

	call printf ("</TABLE>\n")
	call printf ("</BODY>\n")
	call flush (STDOUT)

	call sfree (sp)
end

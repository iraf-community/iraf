include <time.h>		# defines SZ_TIME for datestr
include <error.h>		# defines EA_WARN
include <finfo.h>		# used by tpr_fm_date
include <fset.h>		# used to check whether output is redirected
include <tbset.h>
include "tprint.h"

# tprint -- Program to print tables.
#
# Phil Hodge, 31-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Delete call to tbtext.
# Phil Hodge, 28-Aug-1987  Write date that table was last modified.
# Phil Hodge, 12-Oct-1987  Include LaTeX/TeX option.
# Phil Hodge, 30-Dec-1987  Filename template; tctexp for column names.
# Phil Hodge, 12-Feb-1988  Include option to align columns if format is small.
# Phil Hodge, 30-Mar-1988  Page width = ttyncols; get sp_col & lgroup.
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge, 10-May-1991  Use clpopns instead of clpopnu.
# Phil Hodge, 26-Mar-1992  Remove call to tbtext; use tbtnam instead.
# Phil Hodge, 28-Oct-1992  Set align=false if showhdr=false.
# Phil Hodge,  5-Jul-1993  Include option to print column units.
# Phil Hodge,  3-Feb-1994  Set showunits to false for a text table.
# Phil Hodge, 15-Dec-1994  Increase size of table name from SZ_FNAME to SZ_LINE.
# Phil Hodge, 16-Feb-1995  In tpr_param_pr, print "#" before header lines.
# Phil Hodge,  6-Mar-1995  In tpr_param_pr, print comment for header parameter.
# Phil Hodge, 23-Jun-1995  In tpr_fm_date, get file name using tbparse.
# Phil Hodge,  3-Oct-1995  Replace clgfil calls with tbn... .
# Phil Hodge,  9-Apr-1996  Error check tbtopn and tpr_data_pr;
#	call error if number of columns to print is zero (i.e. if no
#	column was found); flush STDOUT.
# Phil Hodge,  5-Jun-1997  Use single instead of double quotes for keyword
#	text value, if there's an associated comment.
# Phil Hodge, 26-Mar-1998  Add orig_row to par file, to show row number in
#	underlying table in case a row selector was used.
# Phil Hodge,  7-Jun-1999  Allow showunits to be true for a text table;
#	this overrides the change made on 3-Feb-1994;
#	if input is redirected, set input to STDIN without getting cl param.
# Phil Hodge,  9-Aug-1999  Add option = html; modify tpr_space to handle
#	arrays and to just set a flag, rather than actually printing the space.
# Phil Hodge,  3-Jul-2000  In tpr_param_pr, delete leading blanks from the
#	value and comment.
# Phil Hodge, 15-Jul-2009  In tpr_fm_date, remove ttype from the call to
#	tbparse.

procedure tprint()

pointer tp			# pointer to input table descr
pointer sp			# stack pointer
pointer tname			# scratch for table name
pointer columns			# scratch for list of column names
pointer range_string		# string which gives ranges of row numbers
pointer tlist			# for list of input table names
int	pagewidth		# max number of char in line length
int	pagelength		# number of data lines (excl header) per page
int	lgroup			# number of lines to group together on output
bool	prparam, prdata		# print header parameters? data?
bool	showrow			# show row number on output?
bool	orig_row		# show row number from underlying table?
bool	showhdr			# show table name, column names, etc on output?
bool	showunits		# show column units on output?
bool	align			# override print fmt to align col & name?
char	prt_option[SHORT_STRING] # "plain", "latex", "tex"
char	sp_cname[SZ_COLNAME]	# name of column to control spacing
bool	first_table		# false ==> print line to separate tables
int	clgeti(), fstati(), envgeti()
bool	clgetb()
pointer tbnopenp(), tbnopen()
int	tbnget()
pointer tbtopn()

begin
	call smark (sp)
	call salloc (tname, SZ_LINE, TY_CHAR)
	call salloc (columns, SZ_LINE, TY_CHAR)

	if (fstati (STDIN, F_REDIR) == YES)
	    tlist = tbnopen ("STDIN")
	else
	    tlist = tbnopenp ("table")

	# Find out which portions of the table the user wants to print.
	prparam = clgetb ("prparam")
	prdata  = clgetb ("prdata")
	if (!prparam && !prdata) {
	    call sfree (sp)
	    return
	}

	if (prdata) {
	    # Get parameters relevant to printing data portion of table.

	    call salloc (range_string, SZ_LINE, TY_CHAR)

	    # Get page width from ttyncols unless output is redirected.
	    if (fstati (STDOUT, F_REDIR) == YES)
		pagewidth = clgeti ("pwidth")
	    else
		pagewidth = envgeti ("ttyncols")
	    pagelength = clgeti ("plength")
	    showrow = clgetb ("showrow")
	    orig_row = clgetb ("orig_row")
	    call clgstr ("columns", Memc[columns], SZ_LINE)
	    call clgstr ("rows", Memc[range_string], SZ_LINE)
	    align = clgetb ("align")

	    call clgstr ("sp_col", sp_cname, SZ_COLNAME)
	    lgroup = clgeti ("lgroup") + 1	# add one for the space
	}

	call clgstr ("option", prt_option, SHORT_STRING)
	showhdr = clgetb ("showhdr")
	if (showhdr) {
	    showunits = clgetb ("showunits")
	} else {
	    # There's no need to align columns with their names if the names
	    # are not printed.
	    align = false
	    showunits = false
	}

	if (prt_option[1] == 'h')			# HTML
	    call tpr_html_begin()

	# Loop over all table names in the file name template.
	first_table = true
	while (tbnget (tlist, Memc[tname], SZ_LINE) != EOF) {

	    if ( ! first_table ) {
		call printf ("\n")		# blank line between tables
		call flush (STDOUT)
	    }
	    first_table = false

	    # Open the table.
	    iferr {
		tp = tbtopn (Memc[tname], READ_ONLY, 0)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    # Get the full table name (for use by tpr_fm_date),
	    call tbtnam (tp, Memc[tname], SZ_LINE)

	    # Print the name of the table and the date that the table was
	    # last modified.
	    if (showhdr)
		call tpr_fm_date (Memc[tname], prt_option)

	    if (prparam)			# print header parameters
		call tpr_param_pr (tp, prt_option, prdata)

	    if (prdata) {			# print data portion of table
		iferr {
		    call tpr_data_pr (tp, pagewidth, pagelength,
			showrow, orig_row, showhdr, showunits,
			align, sp_cname, lgroup,
			Memc[columns], Memc[range_string], prt_option)
		} then {
		    call tbtclo (tp)
		    call erract (EA_WARN)
		    next
		}
	    }

	    call tbtclo (tp)
	}
	if (prt_option[1] == 'h')			# HTML
	    call tpr_html_end()

	call tbnclose (tlist)
	call sfree (sp)
end



# tpr_fm_date -- Get date of file
# This procedure gets the date that a table was last modified and writes
# the table name and date to STDOUT.

procedure tpr_fm_date (tablename, prt_option)

char	tablename[ARB]		# i: name of table
char	prt_option[ARB]		# i: "plain", "latex", or "tex"
#--
pointer sp
pointer filename		# name of table without brackets
pointer cdfname			# returned by tbparse and ignored
int	hdu			# ignored
long	ostruct[LEN_FINFO]	# contains info about file
long	mtime
char	datestr[SZ_TIME]	# date that table was last modified
char	percent			# the % character
int	junk, tbparse()
int	finfo()

begin
	if (prt_option[1] == 'h')			# HTML
	    return

	call smark (sp)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (cdfname, SZ_FNAME, TY_CHAR)

	# Get file name from table name.
	junk = tbparse (tablename, Memc[filename],
			Memc[cdfname], SZ_FNAME, hdu)

	percent = '%'

	if (finfo (Memc[filename], ostruct) != ERR) {
	    mtime = FI_MTIME(ostruct)
	    call cnvtime (mtime, datestr, SZ_TIME)

	    if (prt_option[1] == 'p') {			# plain print
		call printf ("#  Table %s  %s\n")
		    call pargstr (tablename)
		    call pargstr (datestr)
	    } else {					# LaTeX or TeX
		call printf ("%c")
		    call pargc (percent)		# comment
		call printf (" Table %s  %s\n")
		    call pargstr (tablename)
		    call pargstr (datestr)
	    }
	    call printf ("\n")
	}
	call flush (STDOUT)

	call sfree (sp)
end



# tpr_param_pr -- Print header parameters
# This procedure prints the header parameters on STDOUT.
#
# Phil Hodge,  5-Oct-1987  Subroutine created
# Phil Hodge, 30-Dec-1987  If latex or tex, print % prefix.
# Phil hodge,  9-Mar-1989  Change type of dtype in tbhgnp from char to int.

procedure tpr_param_pr (tp, prt_option, prdata)

pointer tp			# i: pointer to table descriptor
char	prt_option[ARB]		# i: "plain", "latex", or "tex"
bool	prdata			# i: print data?  (not here, though)
#--
pointer sp
pointer value			# scratch for value of parameter (string)
pointer comment			# scratch for comment for parameter
int	dtype			# data type (TY_CHAR, etc)
int	npar			# number of header parameters
int	n			# loop index for parameter number
int	ipp, ipc		# offsets for skipping leading blanks
char	keyword[SZ_KEYWORD]	# buffer for header parameter keyword
char	percent			# the % character
int	tbpsta()

begin
	if (prt_option[1] == 'h') {			# HTML
	    call tpr_html_param (tp)
	    return
	}

	percent = '%'
	npar = tbpsta (tp, TBL_NPAR)
	if (npar > 0) {
	    call smark (sp)
	    call salloc (value, SZ_PARREC, TY_CHAR)
	    call salloc (comment, SZ_PARREC, TY_CHAR)
	    do n = 1, npar {

		# Get the Nth header parameter and comment.
		call tbhgnp (tp, n, keyword, dtype, Memc[value])
		call tbhgcm (tp, keyword, Memc[comment], SZ_PARREC)
		ipp = 0
		while (Memc[value+ipp] == ' ')
		    ipp = ipp + 1
		ipc = 0
		while (Memc[comment+ipc] == ' ')
		    ipc = ipc + 1

		if (prt_option[1] != 'p') {	# LaTeX or TeX, not plain
		    call printf ("%c")
			call pargc (percent)	# comment
		} else if (prdata) {		# plain output, with data
		    call printf ("#K ")		# comment
		}

		# Print the keyword and value, and possibly a comment.
		if (Memc[comment+ipc] == EOS) {	# no comment to print

		    call printf ("%-8s %s\n")
			call pargstr (keyword)
			call pargstr (Memc[value+ipp])

		} else if (dtype == TY_CHAR) {

		    # Enclose value in quotes to distinguish from comment.
		    call printf ("%-8s '%s'  %s\n")
			call pargstr (keyword)
			call pargstr (Memc[value+ipp])
			call pargstr (Memc[comment+ipc])

		} else {

		    # Numeric; no quotes needed.
		    call printf ("%-8s %s  %s\n")
			call pargstr (keyword)
			call pargstr (Memc[value+ipp])
			call pargstr (Memc[comment+ipc])
		}
	    }
	    call sfree (sp)
	    call printf ("\n")
	}
	call flush (STDOUT)
end



# tpr_data_pr -- Print table data
# This procedure prints the data portion of a table on STDOUT.
#
# Phil Hodge,  5-Oct-1987  Subroutine created
# Phil Hodge, 12-Feb-1988  Include option to align columns if format is small.
# Phil Hodge, 30-Mar-1988  Get column to control spacing of printout.

procedure tpr_data_pr (tp, pagewidth, pagelength,
		showrow, orig_row, showhdr, showunits,
		align, sp_cname, lgroup,
		columns, range_string, prt_option)

pointer tp			# i: pointer to table descriptor
int	pagewidth		# i: page width
int	pagelength		# i: number of lines of table per page
bool	showrow			# i: print row number?
bool	orig_row		# i: show row number from underlying table?
bool	showhdr			# i: print column names, etc?
bool	showunits		# i: print column units?
bool	align			# i: override print fmt to align col & name?
char	sp_cname[SZ_COLNAME]	# i: column to control spacing
int	lgroup			# i: print blank line after this many lines
char	columns[ARB]		# i: list of names of columns to be printed
char	range_string[ARB]	# i: range of row numbers to print
char	prt_option[ARB]		# i: "plain", "latex", or "tex"
#--
pointer sp
pointer cptr			# scratch for array of column pointers
pointer s_cp			# pointer to column to control spacing
int	nrows, ncols		# number of rows and columns in table
int	ncp			# number of columns to print (may be < ncols)
int	k			# loop index
int	tbpsta()

begin
	nrows = tbpsta (tp, TBL_NROWS)
	ncols = tbpsta (tp, TBL_NCOLS)

	if ((nrows < 1) || (ncols < 1)) {
	    call eprintf ("table is empty\n")
	    return				# nothing more to do
	}

	# Allocate enough space for storing a descriptor for each column.
	call smark (sp)
	call salloc (cptr, ncols, TY_POINTER)

	# Get column pointers for all columns that are to be printed.
	call tctexp (tp, columns, ncols, ncp, Memi[cptr])

	# Check whether there is a column to control spacing.
	k = 1
	while ((sp_cname[k] == ' ' || sp_cname[k] == '\t') &&
		(sp_cname[k] != EOS) && (k <= SZ_COLNAME))
	    k = k + 1
	if (sp_cname[k] != EOS) {
	    call tbcfnd1 (tp, sp_cname[k], s_cp)
	    if (s_cp == NULL) {
		call eprintf ("WARNING:  column `%s' for spacing not found\n")
		    call pargstr (sp_cname)
	    }
	} else {
	    s_cp = NULL
	}

	if (ncp > 0) {
	    # Print the values in the table.
	    if (prt_option[1] == 'p') {			# plain printing
		call tpr_plain_pr (tp, Memi[cptr], ncp, s_cp, lgroup,
			range_string, pagewidth, pagelength,
			showrow, orig_row, showhdr, showunits, align)
	    } else if (prt_option[1] == 'h') {		# html table
		call tpr_html_pr (tp, Memi[cptr], ncp, s_cp, lgroup,
			range_string, pagelength,
			showrow, orig_row, showhdr, showunits)
	    } else {					# LaTeX or TeX
		call tpr_latex_pr (tp, Memi[cptr], ncp, s_cp, lgroup,
			range_string, pagewidth, pagelength,
			showrow, orig_row, showhdr, showunits, prt_option)
	    }
	    call flush (STDOUT)
	} else {
	    call error (1, "column not found")
	}
	call sfree (sp)
end



# tpr_space -- print line separator
# Check whether we should print a blank line (or other line separator) if
# the value in the designated column has changed since the last call to
# this routine or if a group of lgroup lines has been printed.
# The groups of lines are counted starting at the beginning of each
# page; this makes a difference if lgroup does not divide pagelength.
# If any column being printed contains arrays, then lgroup is applied
# to element number, not to line number.
#
# If lgroup is one then it will be ignored; if s_cp is NULL then
# the column values will be ignored.
# When linenum is zero, the "previous" column value is initialized
# with the current value.

procedure tpr_space (tp, s_cp, lgroup,
		rownum, element, max_nelem, pagelength, linenum, s_flag)

pointer tp			# i: pointer to table descriptor
pointer s_cp			# i: pointer to column to control spacing
int	lgroup			# i: print blank after this many lines
int	rownum			# i: number of current row
int	element			# i: array element number
int	max_nelem		# i: max value for element
int	pagelength		# i: number of data lines per page
int	linenum			# i: number of lines that have been printed
int	s_flag			# o: YES if we should print a line for spacing
#--
pointer sp
pointer current			# scratch for value of column in current row
int	lpage			# number of lines already printed on this page
char	previous[SZ_LINE]	# value of column in previous row
bool	do_compare		# true if we should compare column values
bool	strne()
int	nelem, tbcigi(), junk, tbagtt()
errchk	tbegtt, tbagtt

begin
	s_flag = NO				# may be changed later

	if (lgroup <= 1 && s_cp == NULL)
	    return

	if (pagelength > 0)
	    lpage = mod (linenum, pagelength) + 1
	else
	    lpage = linenum + 1

	if (s_cp != NULL)
	    nelem = tbcigi (s_cp, TBL_COL_LENDATA)
	else
	    nelem = 1

	# If we're at the beginning of a page, get the current value
	# of the column and save it as "previous".  That's all.
	if (lpage == 1) {
	    if (s_cp != NULL) {
		if (nelem > 1) {
		    if (element <= nelem) {
			junk = tbagtt (tp, s_cp, rownum, previous, SZ_LINE,
				element, 1)
		    }
		} else {
		    call tbegtt (tp, s_cp, rownum, previous, SZ_LINE)
		}
	    }
	    return
	}

	# Have we printed a group of lines?  If so, set the flag to indicate
	# that we should print a blank line.
	# If we're printing arrays, apply lgroup to the array elements instead
	# of to the rows, but in this case also print a space between each row
	# (i.e. when element is one), except at the top of a page.
	# Note:  The value of lgroup is one more than the parameter value,
	# so that it can be used with mod on linenum, because linenum gets
	# incremented when a blank line is printed.  But here we want to use
	# lgroup with element, which doesn't get incremented.  That's why
	# we subtract one from lgroup in "mod (element, lgroup-1)".
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
	if (s_cp != NULL) {
	    if (s_flag == YES) {
		# If we already know we need to print a space, we don't have
		# to compare current and previous values, but we still must
		# save current value as "previous".
		if (nelem > 1 && element <= nelem) {
		    junk = tbagtt (tp, s_cp, rownum, previous, SZ_LINE,
				element, 1)
		} else if (element == 1) {
		    call tbegtt (tp, s_cp, rownum, previous, SZ_LINE)
		}		# else we already have the value
	    } else {
		# Get current value, and compare it with previous value.
		call smark (sp)
		call salloc (current, SZ_LINE, TY_CHAR)
		do_compare = true			# may be reset
		if (nelem > 1 && element <= nelem) {
		    junk = tbagtt (tp, s_cp, rownum, Memc[current], SZ_LINE,
				element, 1)
		} else if (element == 1) {
		    call tbegtt (tp, s_cp, rownum, Memc[current], SZ_LINE)
		} else {
		    do_compare = false
		}
		if (do_compare && strne (Memc[current], previous)) {
		    # Set flag; save current value as previous value.
		    s_flag = YES
		    call strcpy (Memc[current], previous, SZ_LINE)
		}
		call sfree (sp)
	    }
	}
end

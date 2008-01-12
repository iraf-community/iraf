include <tbset.h>

# tdump -- Program to dump a table.
# This differs from tprint in several ways:  column names and row numbers
# are not printed, and all columns for a given row are printed (possibly on
# several lines) before moving on to the next row.  Also, g format is used
# for floating-point numbers (%15.7g for real, %24.16g for double) regardless
# of the format specification for the column.  This is to prevent loss of
# precision.
#
# Phil Hodge, 31-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Modify d_gt_col_ptr for datatype=-n for char string.
# Phil Hodge, 30-Dec-1987  Use tctexp for column names.
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge, 21-Dec-1988  Also print column descrip; use g format for data.
# Phil Hodge,  9-Mar-1989  Change type of dtype in tbhgnp from char to int.
# Phil Hodge,  9-Jul-1991  Rename parameter pagwidth to pwidth.
# Phil Hodge,  2-Apr-1993  Include short datatype in td_col_def.
# Phil Hodge,  2-Jun-1994  In td_col_ptr, include newline in warning message.
# Phil Hodge, 12-Dec-1994  Include array size in column definitions;
#			increase SZ_DTYPE from 9 to 29;
#			dump all elements if column is an array.
# Phil Hodge, 15-Dec-1994  Increase size of file names from SZ_FNAME to SZ_LINE.
# Phil Hodge, 13-Jan-1995  Change calling sequence of inquotes.
# Phil Hodge, 19-Jul-1995  Add tp to calling sequence of tl_dtype in td_col_def.
# Phil Hodge,  4-Apr-1996  In td_p_data, start each array at beginning of line;
#			change formats for real & double to %13.7g, %22.16g.
# Phil Hodge,  5-Jun-1997  If keywords are to be printed, also print comments.
# Phil Hodge, 20-Jul-1998  Print '' instead of blank for null keywords.
# Phil Hodge, 22-Jul-1998  Left justify strings and boolean elements.
# Phil Hodge,  2-Nov-2000  Use pwidth < 1 to disable the test on page width.
# Phil Hodge, 15-May-2002  Use a specific format for int and short columns;
#			this was needed because for x or o format the printed
#			values could be misleading.

define  SZ_FMT    	16	# size of string containing print format
define	FMT_REAL    "%13.7g"	# format for printing a real
define	SPACE_REAL	13	# space required for printing a real
define	FMT_DBL     "%22.16g"	# format for printing a double
define	SPACE_DBL	22	# space required for printing a double
define	FMT_INT     "%11d"	# format for printing an int
define	SPACE_INT	11	# space required for printing an int
define	FMT_SHORT   "%5d"	# format for printing a short
define	SPACE_SHORT	 5	# space required for printing a short
define	MAX_RANGES (SZ_LINE/2)	# max number of ranges of row numbers
define  SZ_DTYPE  	29	# size of string containing column data type
define	SZ_LBUF   2 * SZ_LINE + 1

procedure tdump()
#--
pointer tp			# pointer to input table descr
pointer cptr			# scratch for array of column pointers
pointer tname			# scratch for table name
pointer cname, pname, dname	# scratch for names of output files
pointer upar			# scratch for header keyword value
pointer comment			# scratch for header keyword comment
pointer datatype		# scr for array of data types of columns
pointer nelem			# scr for array of array lengths of columns
pointer len_fmt			# scr for array of lengths of print formats
pointer pformat			# scratch for array of print formats
pointer columns			# list of columns to be dumped
pointer r_str			# string which gives ranges of row numbers
char	keyword[SZ_KEYWORD]	# buffer for user parameter keyword
char	char_type		# data type as a letter (t, b, i, r, d)
pointer sp			# stack pointer
int	fd			# file descr for output user param, data
int	n			# loop index for user parameters
int	dtype			# data type (TY_CHAR, etc)
int	npar			# number of user parameters
int	nrows, ncols		# number of rows and columns in table
int	nprint			# number of columns to print (may be < ncols)
int	pagewidth		# page width
bool	prcoldef		# print column definitions?
bool	prparam, prdata		# print user parameters? data?
pointer tbtopn()
int	open(), clgeti(), tbpsta()

begin
	call smark (sp)
	call salloc (tname, SZ_LINE, TY_CHAR)
	call salloc (cname, SZ_LINE, TY_CHAR)
	call salloc (pname, SZ_LINE, TY_CHAR)
	call salloc (dname, SZ_LINE, TY_CHAR)
	call clgstr ("table", Memc[tname], SZ_LINE)

	# Get the names of the output files.  If a name is null, don't
	# write the corresponding portion of the table.
	call clgstr ("cdfile", Memc[cname], SZ_LINE)
	call clgstr ("pfile", Memc[pname], SZ_LINE)
	call clgstr ("datafile", Memc[dname], SZ_LINE)
	prcoldef = (Memc[cname] != EOS)
	prparam = (Memc[pname] != EOS)
	prdata = (Memc[dname] != EOS)
	if (!prcoldef && !prparam && !prdata) {
	    call sfree (sp)			# nothing to do
	    return
	}

	tp = tbtopn (Memc[tname], READ_ONLY, 0)

	if (prcoldef || prdata) {

	    # If we are to print column definitions and/or data,
	    # allocate memory and get list of columns.

	    call salloc (columns, SZ_LINE, TY_CHAR)
	    call clgstr ("columns", Memc[columns], SZ_LINE)
	    ncols = tbpsta (tp, TBL_NCOLS)

	    # Allocate enough scratch space for printing all columns.
	    call salloc (cptr, ncols, TY_POINTER)
	    call salloc (len_fmt, ncols, TY_INT)
	    call salloc (datatype, ncols, TY_INT)
	    call salloc (nelem, ncols, TY_INT)
	}

	if (prcoldef) {

	    # Open the output file for the column definitions.
	    fd = open (Memc[cname], NEW_FILE, TEXT_FILE)

	    # Print column definitions.
	    call td_col_def (tp, fd, Memc[columns], Memi[cptr])

	    call close (fd)		# column definitions have been written
	}

	if (prparam) {

	    # Print header parameters.
	    npar = tbpsta (tp, TBL_NPAR)
	    if (npar > 0) {
		fd = open (Memc[pname], NEW_FILE, TEXT_FILE)
		call salloc (upar, SZ_PARREC, TY_CHAR)
		call salloc (comment, SZ_PARREC, TY_CHAR)
		do n = 1, npar {
		    # Get the Nth user parameter, and print it.
		    call tbhgnp (tp, n, keyword, dtype, Memc[upar])
		    call tbhgcm (tp, keyword, Memc[comment], SZ_PARREC)
		    switch (dtype) {
		    case TY_REAL:
			char_type = 'r'
		    case TY_INT:
			char_type = 'i'
		    case TY_DOUBLE:
			char_type = 'd'
		    case TY_BOOL:
			char_type = 'b'
		    default:
			char_type = 't'
		    }
		    if (keyword[1] == EOS) {
			call fprintf (fd, "''      ")
		    } else {
			call fprintf (fd, "%-8s")
			    call pargstr (keyword)
		    }
		    call fprintf (fd, " %c")
			call pargc (char_type)
		    if (Memc[comment] == EOS) {
			call fprintf (fd, " %s\n")
			    call pargstr (Memc[upar])
		    } else {				# also print comment
			if (char_type == 't') {
			    call fprintf (fd, " '%s'")	# enclose text in quotes
				call pargstr (Memc[upar])
			} else {
			    call fprintf (fd, " %s")	# no quotes needed
				call pargstr (Memc[upar])
			}
			call fprintf (fd, "  %s\n")
			    call pargstr (Memc[comment])
		    }
		}
		call close (fd)		# header parameters have been written
	    }
	}

	if (prdata) {

	    # Print data portion of table.
	    nrows = tbpsta (tp, TBL_NROWS)

	    if ((nrows < 1) || (ncols < 1)) {
		call eprintf ("table is empty\n")
		call tbtclo (tp)
		call sfree (sp)
	        return				# nothing more to do
	    }
	    # Open the output file for the table data.
	    fd = open (Memc[dname], NEW_FILE, TEXT_FILE)

	    call salloc (r_str, SZ_LINE, TY_CHAR)
	    call clgstr ("rows", Memc[r_str], SZ_LINE)

	    pagewidth = clgeti ("pwidth")
	    if (IS_INDEF(pagewidth))
		pagewidth = -1		# no limit on page width

	    # Get column pointers, formats, etc for all columns that are
	    # to be printed.
	    call td_col_ptr (tp, Memc[columns], pagewidth, Memi[cptr],
		Memi[len_fmt], Memi[datatype], Memi[nelem], nprint)

	    if (nprint > 0) {
		# Allocate scratch space for print format.  (one char for EOS)
		call salloc (pformat, (SZ_FMT+1)*nprint, TY_CHAR)
		# Print the values in the table.
		call td_p_data (tp, fd, Memi[cptr], Memc[r_str],
			Memi[len_fmt], Memi[datatype], Memi[nelem],
			Memc[pformat], pagewidth, nprint)
	    }
	    call close (fd)		# data values have been printed
	}
	call tbtclo (tp)
	call sfree (sp)
end



# td_col_def -- print column definitions
# This routine prints the column name, data type, print format, and units
# for all columns that were specified by the user.

procedure td_col_def (tp, fd, columns, cptr)

pointer tp			# i: pointer to table descriptor
int	fd			# i: fd for output file
char	columns[ARB]		# i: list of columns to be dumped
pointer cptr[ARB]		# o: array of pointers to column descriptors
#--
pointer sp
pointer cname, cunits, cfmt	# pointers to scratch space for column info
char	chartyp[SZ_DTYPE]	# data type expressed as a string
int	ncols			# the total number of columns in the table
int	nprint			# number of columns to print
int	dtype			# data type of a column
int	nelem			# array length
int	lenformat		# (ignored)
int	colnum			# column number (ignored)
int	k			# loop index
int	tbpsta()

begin
	call smark (sp)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (cunits, SZ_FNAME, TY_CHAR)
	call salloc (cfmt, SZ_COLFMT, TY_CHAR)

	ncols = tbpsta (tp, TBL_NCOLS)

	# Get column pointers for all columns that are to be dumped.
	call tctexp (tp, columns, ncols, nprint, cptr)

	# Do for each column that is to be printed.
	do k = 1, nprint {
	    call tbcinf (cptr[k],
			colnum, Memc[cname], Memc[cunits], Memc[cfmt],
			dtype, nelem, lenformat)

	    # Enclose column name in quotes if it contains embedded
	    # or trailing blanks.
	    call inquotes (Memc[cname], Memc[cname], SZ_FNAME, YES)
	    call fprintf (fd, "%-16s")		# but name can be longer
		call pargstr (Memc[cname])

	    # Print data type.  First convert integer data type code to a
	    # character string, and append info about array size if > 1.
	    call tl_dtype (tp, cptr[k], dtype, nelem, chartyp, SZ_DTYPE)
	    call fprintf (fd, " %-8s")
		call pargstr (chartyp)

	    # Print the format for display.
	    call fprintf (fd, " %8s")
		call pargstr (Memc[cfmt])

	    # Print column units.  Ignore trailing blanks.
	    call inquotes (Memc[cunits], Memc[cunits], SZ_FNAME, NO)
	    call fprintf (fd, "  %s")
		call pargstr (Memc[cunits])
	    call fprintf (fd, "\n")		# end of line for each column
	}
	call sfree (sp)
end


# td_col_ptr -- get column pointers
# This routine gets an array of pointers to the descriptors of those
# columns that are to be printed, plus other info.

procedure td_col_ptr (tp, columns, pagewidth,
		cptr, len_fmt, datatype, nelem, nprint)

pointer tp			# i: pointer to table descriptor
char	columns[ARB]		# i: list of columns to be dumped
int	pagewidth		# i: page width (to make sure it's wide enough)
pointer cptr[ARB]		# o: array of pointers to column descriptors
int	len_fmt[ARB]		# o: length of print format for each column
int	datatype[ARB]		# o: data type for each column
int	nelem[ARB]		# o: array length of each column
int	nprint			# o: number of columns to print
#--
char	colname[SZ_COLNAME]	# column name for possible error message
int	ncols			# total number of columns in the table
int	k			# loop index
int	tbpsta(), tbcigi()

begin
	ncols = tbpsta (tp, TBL_NCOLS)

	# Get column pointers for all columns that are to be dumped.
	call tctexp (tp, columns, ncols, nprint, cptr)

	# For each column that is to be printed, get the length of the print
	# format, and if the column type is string then increase the length
	# of the print format by two for possible enclosing quotes.
	do k = 1, nprint {

	    datatype[k] = tbcigi (cptr[k], TBL_COL_DATATYPE)
	    nelem[k] = tbcigi (cptr[k], TBL_COL_LENDATA)

	    if (datatype[k] == TY_REAL)
		len_fmt[k] = SPACE_REAL
	    else if (datatype[k] == TY_DOUBLE)
		len_fmt[k] = SPACE_DBL
	    else if (datatype[k] == TY_INT)
		len_fmt[k] = SPACE_INT
	    else if (datatype[k] == TY_SHORT)
		len_fmt[k] = SPACE_SHORT
	    else
		len_fmt[k] = tbcigi (cptr[k], TBL_COL_FMTLEN)

	    if (datatype[k] < 0)			# char string column
		len_fmt[k] = len_fmt[k] + 2

	    if (pagewidth > 0 && len_fmt[k] > pagewidth) {
		call tbcigt (cptr[k], TBL_COL_NAME, colname, SZ_COLNAME)
		call eprintf ("Page width is too small for column `%s'.\n")
		    call pargstr (colname)
	    }
	}
end


# td_p_data -- print the contents of the table
# The data in the table are printed one row at a time.

procedure td_p_data (tp, fd, cptr, range_string,
	len_fmt, datatype, nelem,
	pformat, pagewidth, nprint)

pointer tp			# i: pointer to table descriptor
int	fd			# i: fd for output file
pointer cptr[nprint]		# i: array of pointers to column descriptors
char	range_string[ARB]	# i: string which gives ranges of row numbers
int	datatype[nprint]	# i: array of flags:  true if column is a string
int	nelem[ARB]		# i: array length of each column
int	len_fmt[nprint]		# i: array of lengths of print formats
char	pformat[SZ_FMT,nprint]	# io: scratch space for print formats
int	pagewidth		# i: page width
int	nprint			# i: number of columns to print
#--
pointer sp
pointer lbuf			# scratch space for line buffer
double	dbuf			# buffer for double-precision value
real	rbuf			# buffer for single-precision value
int	ibuf			# buffer for integer value
short	sbuf			# buffer for short value
int	nrows			# number of rows in the table
int	rownum, k		# loop indices for row, column
int	j			# loop index for array element
int	line_len		# current line length
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
bool	done			# flag for terminating loop
int	decode_ranges(), get_next_number()
int	tbpsta(), tbagtr(), tbagtd(), tbagti(), tbagts(), tbagtt()
string	MISSING "error reading data from table"

begin
	nrows = tbpsta (tp, TBL_NROWS)

	if (decode_ranges (range_string, ranges, MAX_RANGES, nvalues) != 0) {
	    call eprintf ("bad range of row numbers\n")
	    return
	}

	call smark (sp)
	call salloc (lbuf, SZ_LBUF, TY_CHAR)

	# This section gets the print format for each column.  The
	# format is just "%Ns" or "%-Ns".
	do k = 1, nprint {
	    pformat[1,k] = '%'
	    if (datatype[k] < 0 || datatype[k] == TY_BOOL) {
		call sprintf (pformat[2,k], SZ_FMT-1, "-%ds")	# left justify
		    call pargi (len_fmt[k])
	    } else {
		call sprintf (pformat[2,k], SZ_FMT-1, "%ds")
		    call pargi (len_fmt[k])
	    }
	}

	# This section prints the data.
	rownum = 0				# initialize get_next_number
	line_len = 0
	done = false
	while ( !done ) {

	    stat = get_next_number (ranges, rownum)
	    if ((stat == EOF) || (rownum > nrows)) {
		done = true

	    } else {

		# Print values in current row.  The loop on k is for each
		# column that is to be printed.
		do k = 1, nprint {

		    # If the current column contains arrays, print each
		    # element, and start at the beginning of the line.
		    if (nelem[k] > 1 && line_len > 0) {
			call fprintf (fd, "\n")
			line_len = 0		# reset after newline
		    }
		    do j = 1, nelem[k] {

			# If we have previously printed something on the
			# current line, print either a space or newline,
			# depending on how close we are to the end of the line.
			if (line_len > 1) {
			    if (pagewidth > 0 &&
					line_len + len_fmt[k] >= pagewidth) {
				# need to start a new line
				call fprintf (fd, "\n")
				line_len = 0
			    } else {
				# continue on current line
				call fprintf (fd, " ")
				line_len = line_len + 1
			    }
			}

			if (datatype[k] == TY_REAL) {
			    if (tbagtr (tp, cptr[k], rownum, rbuf, j, 1) < 1)
				call error (1, MISSING)
			    call sprintf (Memc[lbuf], SZ_LBUF, FMT_REAL)
				call pargr (rbuf)
			} else if (datatype[k] == TY_DOUBLE) {
			    if (tbagtd (tp, cptr[k], rownum, dbuf, j, 1) < 1)
				call error (1, MISSING)
			    call sprintf (Memc[lbuf], SZ_LBUF, FMT_DBL)
				call pargd (dbuf)
			} else if (datatype[k] == TY_INT) {
			    if (tbagti (tp, cptr[k], rownum, ibuf, j, 1) < 1)
				call error (1, MISSING)
			    call sprintf (Memc[lbuf], SZ_LBUF, FMT_INT)
				call pargi (ibuf)
			} else if (datatype[k] == TY_SHORT) {
			    if (tbagts (tp, cptr[k], rownum, sbuf, j, 1) < 1)
				call error (1, MISSING)
			    call sprintf (Memc[lbuf], SZ_LBUF, FMT_SHORT)
				call pargs (sbuf)
			} else {
			    if (tbagtt (tp, cptr[k], rownum,
					Memc[lbuf], SZ_LBUF, j, 1) < 1)
				call error (1, MISSING)
			}
			# If the value is a string, enclose in quotes if
			# there are embedded blanks (ignore trailing blanks).
			if (datatype[k] < 0)
			    call inquotes (Memc[lbuf], Memc[lbuf], SZ_LINE, NO)
			call fprintf (fd, pformat[1,k])
			    call pargstr (Memc[lbuf])

			# Add width of current column.
			line_len = line_len + len_fmt[k]
		    }
		}
		call fprintf (fd, "\n")		# end of current row
		line_len = 0
	    }
	}
	call sfree (sp)
end

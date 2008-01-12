include <tbset.h>

define	TM_MERGE	1	# option = merge
define	TM_APPEND	2	# option = append rows

# tmerge -- Merge or append tables
#
# Two or more input tables are combined into one output table.
#
# If the option is to append then there will be a separate output row for
# each row of each input table.  Columns with the same name in different
# input tables will be written into the same output column, but no data
# will be overwritten because they are written into different rows.
#
# If the option is to merge then each input table is written beginning
# with row one of the output table.  There will be as many output rows as
# the largest number of rows in the input tables.  If there are columns
# with the same name in different input tables then values will be over-
# written as they are put into the output table.  The exception to this
# is text tables if the option allcols=yes; in this case the original
# column names "c1", "c2", etc. will be ignored, and new names will be
# created consisting of the column number in the _output_ table.
#
# Phil Hodge,  2-Sep-1987  Task created.
# Phil Hodge,  7-Sep-1988  Change parameter name for input table.
# Phil Hodge, 16-Mar-1992  Include text as a valid table type.
# Phil Hodge, 16-Apr-1992  Only call tbcdef if column does not exist.
# Phil Hodge, 29-Jun-1992  Allow tbltype = default.
# Phil Hodge, 25-Feb-1993  Create new column names for text tables if
#		allcols=yes; in tm_cp_col, use tbcigi & tbcigt instead of
#		tbcinf, and get data type from output table instead of input.
# Phil Hodge,  2-Apr-1993  In tm_cp_col, include short datatype.
# Phil Hodge, 10-Nov-1993  In tm_mkname, use strcat instead of sprintf.
# Phil Hodge, 29-Aug-1994  Modify to use tbrcsc instead of copying columns.
# Phil Hodge,  3-Oct-1995  Modify for FITS tables.
# Phil Hodge, 28-May-1996  Rewrite tm_mkname to use parts of dir, table name.
# Phil Hodge,  8-Apr-1999  In tm_create, call tbfpri.
# Phil hodge, 16-Apr-1999  In tm_create, use tbttyp instead of tbparse.
# Phil hodge, 18-Jun-1999  Change the way column names are created for
#		text tables:  use c<N>, N = column number in output table;
#		tm_mkname was rewritten; tm_ch_colnames was eliminated.

procedure tmerge()

pointer otp			# output table descriptor
pointer sp
pointer tablist			# scratch for input list of table names
pointer outtbl			# scratch for output table name
pointer ttype			# scratch for table type ("row" or "column")
pointer s_option		# scratch for "append" or "merge"
int	option			# TM_MERGE or TM_APPEND
pointer list			# filename template list
pointer n_outcols		# cumulative number of output columns numbers
bool	allcols			# no ==> take col def only from first input tbl
pointer tbnopen()
int	tbnlen()
bool	clgetb()

begin
	call smark (sp)
	call salloc (tablist, SZ_LINE, TY_CHAR)
	call salloc (outtbl, SZ_LINE, TY_CHAR)
	call salloc (ttype, SZ_LINE, TY_CHAR)
	call salloc (s_option, SZ_LINE, TY_CHAR)

	call clgstr ("intable", Memc[tablist], SZ_LINE)
	call clgstr ("outtable", Memc[outtbl], SZ_LINE)

	call clgstr ("option", Memc[s_option], SZ_FNAME)
	if (Memc[s_option] == 'm')
	    option = TM_MERGE
	else if (Memc[s_option] == 'a')
	    option = TM_APPEND
	else
	    call error (1, "unrecognized 'option'")
	allcols = clgetb ("allcols")
	call clgstr ("tbltype", Memc[ttype], SZ_FNAME)

	list = tbnopen (Memc[tablist])

	# For input table i, n_outcols[i] will be the number of columns
	# defined in the output table prior to processing table i.
	call calloc (n_outcols, tbnlen (list), TY_INT)

	# Create output table.
	call tm_create (list, Memc[outtbl], option, allcols, Memc[ttype],
		otp, Memi[n_outcols])

	# Copy contents of all input tables to output table.
	call tm_copy (list, otp, Memi[n_outcols], option, allcols)

	call tbtclo (otp)
	call tbnclose (list)
	call sfree (sp)
end


# tm_create -- create output table
# This routine opens (and then closes) each input table in order to
# count header parameters and rows, and define columns.  The output table
# is initialized, parameters are set, and the table is created.

procedure tm_create (list, outtbl, option, allcols, ttype,
		otp, n_outcols)

pointer list			# i: filename template descriptor
char	outtbl[ARB]		# i: name of output table
char	ttype[ARB]		# i: "row" or "column" ordered
int	option			# i: merge or append
bool	allcols			# i: use all input columns?
pointer otp			# o: output table pointer
int	n_outcols[ARB]		# o: cumulative count of output column numbers
#--
pointer itp			# input table descriptor
pointer sp
pointer intbl			# scratch for name of an input table
pointer scratch
int	num_par			# number of header parameters
int	itab			# input table number
int	nrows			# sum of number of rows in each input table
int	ncols			# cumulative number of columns in output table
int	allrows			# number of rows to allocate in output
int	extracol		# space for extra columns to allocate in output
int	exttype			# type of output table implied by extension
int	exists, tbttyp()	# exists is ignored
int	phu_copied		# set by tbfpri and ignored
pointer tbtopn()
int	clgeti(), tbnget(), tbpsta()

begin
	call smark (sp)
	call salloc (intbl, SZ_PATHNAME, TY_CHAR)
	call salloc (scratch, SZ_PATHNAME, TY_CHAR)

	call tbnrew (list)				# rewind list
	if (tbnget (list, Memc[intbl], SZ_PATHNAME) == EOF)	# first name
	    call error (1, "no input")
	# If the input and output names are the same post an error.
	call tm_same_name (Memc[intbl], outtbl)
	itp = tbtopn (Memc[intbl], READ_ONLY, 0)	# open first input table
	call tbfpri (Memc[intbl], outtbl, phu_copied)	# copy primary header
	otp = tbtopn (outtbl, NEW_COPY, itp)		# open output table

	# Count header parameters and rows, then close first input table.
	num_par = tbpsta (itp, TBL_NPAR)
	nrows   = tbpsta (itp, TBL_NROWS)
	call tbtclo (itp)

	# Set parameters.

	call tbpset (otp, TBL_MAXPAR, num_par)

	# Check whether the extension of the output file implies a FITS
	# file, while tbltype implies another type.
	exttype = tbttyp (outtbl, exists)
	if (exttype == TBL_TYPE_FITS) {
	    if (ttype[1] == 'r' || ttype[1] == 'c' || ttype[1] == 't') {
		call eprintf (
		"warning:  Extension of output name implies a FITS file,\n")
		call eprintf (
		"but tbltype specifies `%s'; output will be FITS.\n")
		    call pargstr (ttype)
	    }
	}

	if (exttype != TBL_TYPE_FITS) {

	    if (ttype[1] == 'r') {		# row-ordered stsdas format
		call tbpset (otp, TBL_WHTYPE, TBL_TYPE_S_ROW)
		extracol = clgeti ("extracol")
		# Increase allocation of space for columns.
		if (extracol > 0)
		    call tbpset (otp, TBL_INCR_ROWLEN, extracol)
	    } else if (ttype[1] == 'c') {	# column-ordered stsdas format
		call tbpset (otp, TBL_WHTYPE, TBL_TYPE_S_COL)
		allrows = clgeti ("allrows")
		allrows = max (allrows, nrows)	# user may want more rows
		call tbpset (otp, TBL_ALLROWS, allrows)
	    } else if (ttype[1] == 't') {		# text table
		call tbpset (otp, TBL_WHTYPE, TBL_TYPE_TEXT)
	    } else if (ttype[1] == 'd') {	# default -- don't set type
		extracol = clgeti ("extracol")
		if (extracol > 0)
		    call tbpset (otp, TBL_INCR_ROWLEN, extracol)
	    }
	}

	# Open each of the other input tables, define columns, count
	# header parameters and rows, and close the table.
	itab = 1		# we've opened the first input table already
	n_outcols[1] = 0	# no output columns prior to first input table
	while (tbnget (list, Memc[intbl], SZ_PATHNAME) != EOF) {

	    call tm_same_name (Memc[intbl], outtbl)	# check names
	    itp = tbtopn (Memc[intbl], READ_ONLY, 0)
	    itab = itab + 1			# increment input table counter
	    ncols = tbpsta (otp, TBL_NCOLS)	# ncols in output table, so far
	    n_outcols[itab] = n_outcols[itab-1] + ncols
	    if (allcols)
		call tm_def_col (itp, otp, n_outcols[itab])
	    num_par = num_par + tbpsta (itp, TBL_NPAR)
	    if (option == TM_MERGE)
		nrows = max (nrows, tbpsta (itp, TBL_NROWS))
	    else	# append
		nrows = nrows + tbpsta (itp, TBL_NROWS)
	    call tbtclo (itp)
	}

	# Create the output table.
	call tbtcre (otp)
	call sfree (sp)
end


# tm_copy -- copy contents of tables
# This routine opens each of the input tables one at a time, copies
# the header parameters to the output table, copies the data contents,
# and closes each input table.

procedure tm_copy (list, otp, n_outcols, option, allcols)

pointer list			# i: filename template descriptor
pointer otp			# i: output table descriptor
int	n_outcols[ARB]		# i: cumulative count of output column numbers
int	option			# i: merge or append
bool	allcols			# i: passed to tm_cp_col (for text tables)
#--
pointer itp			# input table descriptor
pointer sp
pointer intbl			# scratch for name of an input table
pointer icp			# pointer to column descriptor in input table
pointer ocp			# pointer to one column descriptor in output
pointer icptr			# array of pointers to col descr in input
pointer ocptr			# array of pointers to col descr in output
char	colname[SZ_COLNAME]	# column name
int	itab			# input table number
int	nrows			# number of rows in input table
int	irow, orow		# loop index for row numbers in input, output
int	firstrow		# first row to write in output table
int	ncols			# number of columns in input table
int	nc			# number of columns to copy to output table
int	colnum			# loop index for column number
bool	simple_text_table	# input is a simple text table (no col def)?
pointer tbtopn(), tbcnum()
int	tbnget(), tbpsta()

begin
	call smark (sp)
	call salloc (intbl, SZ_PATHNAME, TY_CHAR)

	firstrow = 1				# initial values
	itab = 0
	call tbnrew (list)			# rewind list

	while (tbnget (list, Memc[intbl], SZ_PATHNAME) != EOF) {

	    itp = tbtopn (Memc[intbl], READ_ONLY, 0)
	    itab = itab + 1			# increment input table counter
	    call tbhcal (itp, otp)		# copy all header parameters
	    nrows = tbpsta (itp, TBL_NROWS)
	    ncols = tbpsta (itp, TBL_NCOLS)

	    simple_text_table = false		# initial value
	    if (tbpsta (itp, TBL_WHTYPE) == TBL_TYPE_TEXT) {
		if (tbpsta (itp, TBL_SUBTYPE) == TBL_SUBTYPE_SIMPLE) {
		    simple_text_table = true
		}
	    }

	    if (nrows > 0) {

		# Column descriptors for output table.
		call malloc (icptr, ncols, TY_POINTER)
		call malloc (ocptr, ncols, TY_POINTER)

		# For each column in the input table, use the column number
		# to get its name.  Using its name, look for it in the
		# current output table.  If it is there, save its column
		# descriptor in the array; otherwise, ignore the column.
		nc = 0				# no columns found yet
		do colnum = 1, ncols {

		    icp = tbcnum (itp, colnum)
		    call tbcigt (icp, TBL_COL_NAME, colname, SZ_COLNAME)

		    # For a text table, if we're taking values from all
		    # columns, assign a new column name.
		    if (allcols && simple_text_table) {
			call tm_mkname (n_outcols[itab] + colnum,
				colname, SZ_COLNAME)
		    }
		    call tbcfnd (otp, colname, ocp, 1)
		    if (ocp != NULL) {
			nc = nc + 1
			Memi[icptr+nc-1] = icp		# nc, not colnum
			Memi[ocptr+nc-1] = ocp
		    }
		}

		# Copy each row.
		orow = firstrow				# initial value
		do irow = 1, nrows {
		    call tbrcsc (itp, otp, Memi[icptr], Memi[ocptr],
				irow, orow, nc)
		    orow = orow + 1
		}
		# Free memory for column descriptors.
		call mfree (ocptr, TY_POINTER)
		call mfree (icptr, TY_POINTER)

		if (option == TM_APPEND)	# else keep firstrow = 1
		    firstrow = firstrow + nrows
	    }
	    call tbtclo (itp)
	}
	call sfree (sp)
end


# tm_same_name -- call error if same names
# This routine appends the default extension and then compares the
# two names to make sure they are different.  An error is posted if
# the names are the same.

procedure tm_same_name (tbl1, tbl2)

char	tbl1[ARB], tbl2[ARB]	# i: the names to be compared
#--
pointer name1, name2		# scratch for names including extension
pointer sp
bool	streq()

begin
	call smark (sp)
	call salloc (name1, SZ_PATHNAME, TY_CHAR)
	call salloc (name2, SZ_PATHNAME, TY_CHAR)
	call tbtext (tbl1, Memc[name1], SZ_PATHNAME)
	call tbtext (tbl2, Memc[name2], SZ_PATHNAME)
	if (streq (Memc[name1], Memc[name2]))
	    call error (1, "input and output names must be different")
	call sfree (sp)
end


# tm_def_col -- define columns
# All columns in the input table are defined in the output table.
# It is not an error for a column to have been previously defined.
# (Note that this routine is only called if allcols is true.)

procedure tm_def_col (itp, otp, prev_ncols)

pointer itp, otp		# i: descriptors for input and output tables
int	prev_ncols		# i: previous number of output columns
#--
pointer colptr, cp		# column descriptors for input & output
char	colname[SZ_COLNAME]	# column name
char	colunits[SZ_COLUNITS]	# units for column
char	colfmt[SZ_COLFMT]	# print format for column
int	dtype[1]		# data type of column
int	lendata[1]		# number of elements (one)
int	lenfmt			# length of print format (ignored)
int	ncols			# number of columns in input table
int	colnum, cn		# column number; cn is ignored
int	duplicate		# count of duplicate columns for text input
bool	simple_text_table	# input is a simple text table (no col def)?
pointer tbcnum()
int	tbpsta()

begin
	ncols = tbpsta (itp, TBL_NCOLS)

	simple_text_table = false		# initial value
	if (tbpsta (itp, TBL_WHTYPE) == TBL_TYPE_TEXT) {
	    if (tbpsta (itp, TBL_SUBTYPE) == TBL_SUBTYPE_SIMPLE) {
		simple_text_table = true
	    }
	}

	duplicate = 0				# initial value

	do colnum = 1, ncols {
	    colptr = tbcnum (itp, colnum)
	    call tbcinf (colptr,
			cn, colname, colunits, colfmt,
			dtype, lendata, lenfmt)

	    # For a simple text table, assign a column name based on the
	    # number of the column we're about to create in the output table.
	    if (simple_text_table)
		call tm_mkname (prev_ncols+colnum, colname, SZ_COLNAME)

	    # Check whether the column already exists ...
	    call tbcfnd (otp, colname, cp, 1)
	    # ... and if not then create it.
	    if (cp == NULL)
		call tbcdef (otp, cp, colname, colunits, colfmt,
			dtype, lendata, 1)
	    else if (simple_text_table)
		duplicate = duplicate + 1
	}

	if (duplicate > 0) {
	    call eprintf (
		"warning:  %d duplicate column names from text table\n")
		call pargi (duplicate)
	}
end


# tm_mkname -- create a column name
# This routine constructs a new name for a column of a text table.
# The name will be "c" followed by the number of the column in the
# output table.

procedure tm_mkname (outcol, colname, maxch)

int	outcol			# i: number of column in output table
char	colname[maxch]		# o: column name
int	maxch			# i: size of colname
#--

begin
	call sprintf (colname, maxch, "c%d")
	    call pargi (outcol)
end

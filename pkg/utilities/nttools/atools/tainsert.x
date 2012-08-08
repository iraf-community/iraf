include <tbset.h>

define	BUFSIZE		1024	# max number of elements copied at one time

# tainsert -- copy a column from one table to an entry in another
# This task inserts an array of values into a row for a column that contains
# array entries.  If the output table exists it will be written to in-place;
# otherwise, it will be created.  The same column name is used in both
# tables.  If the row number is less than one, the output row number will be
# taken from the keyword ORIG_ROW in the input table.
#
# Phil Hodge, 28-Jul-1994  Task created.
# Phil Hodge, 15-Dec-1995  Add nremain, fix while loop on ncopy.
# Phil Hodge,  4-Apr-1996  Remove slen from calling sequence of tbaptr, etc.,
#			for writing indef to extra elements of array.
# Phil Hodge, 30-Jan-1998  Add optional parameters to define new column;
#			call tbhgti as a function, not a subroutine.
# Phil Hodge,  8-Apr-1999  Call tbfpri.
# Phil Hodge, 13-Apr-2000  Add column name to warning message.

procedure tainsert()

pointer intable
pointer outtable
int	row			# row number at which to insert
char	column[SZ_COLNAME]	# name of column to copy
char	outcolumn[SZ_COLNAME]	# name to use for column in output table
int	size			# length of output array for new column
char	colunits[SZ_COLUNITS]	# units for new column
char	colfmt[SZ_COLFMT]	# display format for new column
pointer dtype			# data type of new column
#--
pointer sp
pointer x			# scratch for array of data
pointer nbuf			# scratch for array of null flags
pointer itp, otp		# pointers to table structs
pointer icp, ocp		# pointers to column structs
int	datatype		# data type of column
char	icolname[SZ_COLNAME]	# from tbcinf for input table column
char	icolunits[SZ_COLUNITS]	# from tbcinf, units for column
char	icolfmt[SZ_COLFMT]	# from tbcinf, display format
int	idatatype		# from tbcinf, data type of column
int	colnum, lenfmt		# output from tbcinf and ignored
int	nrows			# number of rows in input table
int	nelem			# input number of rows, output length of array
int	nremain			# number of elements that remain to be copied
int	ncopy			# number of elements to copy at once
int	i			# loop index
int	first, last		# first and last elements (or rows)
int	slen			# length of string to copy
int	phu_copied		# set by tbfpri and ignored
bool	inplace			# true if output table already exists
bool	newcolumn		# true if output column does not already exist
int	delete			# should we delete output table if error?
pointer tbtopn()
int	clgeti(), tbpsta(), tbtacc(), tbcigi(), tbhgti()
bool	isblank()

# INDEF values for use in a calling sequence:
#   (The problem is that INDEFS is an int, not a short; the others may be OK.)
double	undefd
real	undefr
int	undefi
short	undefs

begin
	call smark (sp)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (dtype, SZ_FNAME, TY_CHAR)

	call clgstr ("intable", Memc[intable], SZ_FNAME)
	call clgstr ("outtable", Memc[outtable], SZ_FNAME)
	row = clgeti ("row")
	call clgstr ("column", column, SZ_COLNAME)
	call clgstr ("outcolumn", outcolumn, SZ_COLNAME)

	# The input column name is the default for the output.
	if (isblank (outcolumn))
	    call strcpy (column, outcolumn, SZ_COLNAME)

	# Open input and output tables.
	itp = tbtopn (Memc[intable], READ_ONLY, NULL)
	if (tbtacc (Memc[outtable]) == YES) {
	    otp = tbtopn (Memc[outtable], READ_WRITE, NULL)
	    inplace = true
	} else {
	    call tbfpri (Memc[intable], Memc[outtable], phu_copied)
	    otp = tbtopn (Memc[outtable], NEW_FILE, NULL)
	    inplace = false
	}
	if (inplace)
	    delete = NO
	else
	    delete = YES	# delete output table in case of error

	undefd = INDEFD
	undefr = INDEFR
	undefi = INDEFI
	undefs = INDEFS

	if (row < 1 || IS_INDEFI(row)) {
	    iferr (row = tbhgti (itp, "orig_row"))
		call taex_disaster (itp, otp, NO,
	"row number not specified, and ORIG_ROW not found in intable")
	}

	# This will be the number of elements in the output array,
	# unless the user explicitly specifies a different size.
	nrows = tbpsta (itp, TBL_NROWS)

	# Find input column.
	call tbcfnd (itp, column, icp, 1)
	if (icp == NULL)
	    call taex_disaster (itp, otp, NO, "column not found in input table")

	# Find or create output column.  If we're creating a new column,
	# use the input column as a template, except that the output will be
	# an array of length 'size', which defaults to nrows but can be
	# different if the user specifies a value.  The name of the output
	# column can also be different from the input.
	call tbcfnd (otp, outcolumn, ocp, 1)
	if (ocp == NULL) {
	    # Column not found in output, so create it.
	    call tbcinf (icp, colnum, icolname, icolunits, icolfmt,
		    idatatype, nelem, lenfmt)
	    if (nelem > 1)
		call taex_disaster (itp, otp, NO,
			"column in input table contains arrays")
	    # Get optional parameters if creating new column.
	    size = clgeti ("size")
	    call clgstr ("colunits", colunits, SZ_COLUNITS)
	    call clgstr ("colfmt", colfmt, SZ_COLFMT)
	    call clgstr ("datatype", Memc[dtype], SZ_FNAME)
	    # Assign default values if not specified.
	    if (IS_INDEFI(size) || size < 1)
		size = nrows
	    if (isblank (colunits))
		call strcpy (icolunits, colunits, SZ_COLUNITS)
	    if (isblank (colfmt))
		call strcpy (icolfmt, colfmt, SZ_COLFMT)
	    if (isblank (Memc[dtype])) {
		datatype = idatatype
	    } else {
		# convert e.g. "real" to 6
		call tbbtyp (Memc[dtype], datatype)
	    }
	    call tbcdef (otp, ocp, outcolumn, colunits, colfmt,
		    datatype, size, 1)			# an array
	    newcolumn = true
	} else {
	    newcolumn = false
	}
	if (!inplace)
	    call tbtcre (otp)

	# Get number of elements to copy.
	nelem = tbcigi (ocp, TBL_COL_LENDATA)
	if (nrows > nelem) {
	    call eprintf (
"Warning:  The number of input rows (%d) in column %s\n")
		call pargi (nrows)
		call pargstr (column)
	    call eprintf (
"  is greater than the array size (%d); the extra rows will be ignored.\n")
		call pargi (nelem)
	}
	nremain = min (nrows, nelem)		# total number to copy
	ncopy = min (nremain, BUFSIZE)
	first = 1
	last = ncopy

	# Copy the data.
	datatype = tbcigi (icp, TBL_COL_DATATYPE)
	call salloc (nbuf, ncopy, TY_BOOL)
	if (datatype == TY_REAL) {
	    call salloc (x, ncopy, TY_REAL)
	    while (ncopy > 0) {
		call tbcgtr (itp, icp, Memr[x], Memb[nbuf], first, last)
		call tbaptr (otp, ocp, row, Memr[x], first, ncopy)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_DOUBLE) {
	    call salloc (x, ncopy, TY_DOUBLE)
	    while (ncopy > 0) {
		call tbcgtd (itp, icp, Memd[x], Memb[nbuf], first, last)
		call tbaptd (otp, ocp, row, Memd[x], first, ncopy)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_INT) {
	    call salloc (x, ncopy, TY_INT)
	    while (ncopy > 0) {
		call tbcgti (itp, icp, Memi[x], Memb[nbuf], first, last)
		call tbapti (otp, ocp, row, Memi[x], first, ncopy)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_SHORT) {
	    call salloc (x, ncopy, TY_SHORT)
	    while (ncopy > 0) {
		call tbcgts (itp, icp, Mems[x], Memb[nbuf], first, last)
		call tbapts (otp, ocp, row, Mems[x], first, ncopy)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_BOOL) {
	    call salloc (x, ncopy, TY_BOOL)
	    while (ncopy > 0) {
		call tbcgtb (itp, icp, Memb[x], Memb[nbuf], first, last)
		call tbaptb (otp, ocp, row, Memb[x], first, ncopy)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype < 0) {		# character string
	    slen = -datatype + 3		# a little extra space
	    call salloc (x, slen, TY_CHAR)
	    do i = 1, nelem {
		call tbegtt (itp, icp, i, Memc[x], slen)
		call tbaptt (otp, ocp, row, Memc[x], slen, i, 1)
	    }

	} else {
	    call eprintf ("datatype = %d\n")
		call pargi (datatype)
	    call taex_disaster (itp, otp, delete, "unknown data type")
	}

	# If we wrote to an existing column in an existing table, and the
	# output column array has more elements than input rows, then we
	# should set the remaining elements in this entry to INDEF.
	if (!newcolumn) {
	    if (datatype == TY_REAL) {
		do i = nrows+1, nelem
		    call tbaptr (otp, ocp, row, undefr, i, 1)
	    } else if (datatype == TY_DOUBLE) {
		do i = nrows+1, nelem
		    call tbaptd (otp, ocp, row, undefd, i, 1)
	    } else if (datatype == TY_INT) {
		do i = nrows+1, nelem
		    call tbapti (otp, ocp, row, undefi, i, 1)
	    } else if (datatype == TY_SHORT) {
		do i = nrows+1, nelem
		    call tbapts (otp, ocp, row, undefs, i, 1)
	    } else if (datatype == TY_BOOL) {
		do i = nrows+1, nelem
		    call tbaptb (otp, ocp, row, false, i, 1)
	    } else if (datatype < 0) {
		slen = -datatype
		do i = nrows+1, nelem
		    call tbaptt (otp, ocp, row, "", slen, i, 1)
	    }
	}

	call tbtclo (otp)
	call tbtclo (itp)

	call sfree (sp)
end

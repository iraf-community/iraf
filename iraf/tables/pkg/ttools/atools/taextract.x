include <tbset.h>

define	BUFSIZE		1024	# max number of elements copied at one time

# taextract -- copy an entry from one table to another
# This task extracts an entry at a specified row & column (presumably
# an array of values) and writes it as a column of scalar values to
# another table.  If the output table exists it will be written to in-place;
# otherwise, it will be created.  The same column name is used in both
# tables.  The input row number is written to the header of the output
# table using keyword ORIG_ROW.
#
# Phil Hodge, 28-Jul-1994  Task created.
# Phil Hodge, 15-Dec-1995  Add nremain, fix while loop on ncopy.
# Phil Hodge, 29-Jul-1997  Rename delete to delete_flag to avoid confusion
#			with the delete subroutine.
# Phil Hodge, 30-Jan-1998  Add optional parameters to define new column.
# Phil Hodge,  8-Apr-1999  Call tbfpri.

procedure taextract()

pointer intable
pointer outtable
int	row			# row number at which to extract
char	column[SZ_COLNAME]	# name of column from which to extract
char	outcolumn[SZ_COLNAME]	# name to use for column in output table
char	colunits[SZ_COLUNITS]	# units for new column
char	colfmt[SZ_COLFMT]	# display format for new column
pointer dtype			# data type of new column
#--
pointer sp
pointer x			# scratch for array of data
pointer itp, otp		# pointers to table structs
pointer icp, ocp		# pointers to column structs
int	datatype		# data type of column
char	icolname[SZ_COLNAME]	# from tbcinf for input table column
char	icolunits[SZ_COLUNITS]	# from tbcinf, units for column
char	icolfmt[SZ_COLFMT]	# from tbcinf, display format
int	idatatype		# from tbcinf, data type of column
int	colnum, lenfmt		# output from tbcinf and ignored
int	nelem			# input length of array, output number of rows
int	nremain			# number of elements that remain to be copied
int	ncopy			# number of elements to copy at once
int	i			# loop index
int	first, last		# first and last elements (or rows)
int	slen			# length of string to copy
int	phu_copied		# set by tbfpri and ignored
bool	inplace			# true if output table already exists
bool	newcolumn		# true if output column does not already exist
int	delete_flag		# should we delete output table if error?
pointer tbtopn()
int	clgeti(), tbpsta(), tbtacc(), tbcigi()
int	tbagtr(), tbagtd(), tbagti(), tbagts(), tbagtb(), tbagtt()
bool	isblank()

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
	    delete_flag = NO
	else
	    delete_flag = YES	# delete output table in case of error

	if (row < 1 || row > tbpsta (itp, TBL_NROWS)) {
	    call taex_disaster (itp, otp, NO, "row not found in input table")
	}

	# Find input column.
	call tbcfnd (itp, column, icp, 1)
	if (icp == NULL)
	    call taex_disaster (itp, otp, NO, "column not found in input table")

	# Find or create output column.
	call tbcfnd (otp, outcolumn, ocp, 1)
	if (ocp == NULL) {
	    # Column not found in output.  Create it using the input column
	    # as a template, except that the output will not be an array.
	    # The name might also be different.
	    call tbcinf (icp, colnum, icolname, icolunits, icolfmt,
		    idatatype, nelem, lenfmt)
	    # Get optional parameters if creating new column.
	    call clgstr ("colunits", colunits, SZ_COLUNITS)
	    call clgstr ("colfmt", colfmt, SZ_COLFMT)
	    call clgstr ("datatype", Memc[dtype], SZ_FNAME)
	    # Assign default values if not specified.
	    if (isblank (colunits))
		call strcpy (icolunits, colunits, SZ_COLUNITS)
	    if (isblank (colfmt))
		call strcpy (icolfmt, colfmt, SZ_COLFMT)
	    if (isblank (Memc[dtype]))
		datatype = idatatype
	    else
		call tbbtyp (Memc[dtype], datatype)
	    call tbcdef (otp, ocp, outcolumn, colunits, colfmt,
		    datatype, 1, 1)			# a column of scalars
	    newcolumn = true
	} else {
	    newcolumn = false
	}
	if (!inplace)
	    call tbtcre (otp)

	# Save the row number as a header parameter.
	call tbhadi (otp, "orig_row", row)

	# Get number of elements to copy.
	nelem = tbcigi (icp, TBL_COL_LENDATA)
	nremain = nelem			# initialize to total number to copy
	ncopy = min (nremain, BUFSIZE)
	first = 1
	last = ncopy

	# Copy the data.
	datatype = tbcigi (icp, TBL_COL_DATATYPE)
	if (datatype == TY_REAL) {
	    call salloc (x, ncopy, TY_REAL)
	    while (ncopy > 0) {
		if (tbagtr (itp, icp, row, Memr[x], first, ncopy) < ncopy)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbcptr (otp, ocp, Memr[x], first, last)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_DOUBLE) {
	    call salloc (x, ncopy, TY_DOUBLE)
	    while (ncopy > 0) {
		if (tbagtd (itp, icp, row, Memd[x], first, ncopy) < ncopy)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbcptd (otp, ocp, Memd[x], first, last)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_INT) {
	    call salloc (x, ncopy, TY_INT)
	    while (ncopy > 0) {
		if (tbagti (itp, icp, row, Memi[x], first, ncopy) < ncopy)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbcpti (otp, ocp, Memi[x], first, last)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_SHORT) {
	    call salloc (x, ncopy, TY_SHORT)
	    while (ncopy > 0) {
		if (tbagts (itp, icp, row, Mems[x], first, ncopy) < ncopy)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbcpts (otp, ocp, Mems[x], first, last)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype == TY_BOOL) {
	    call salloc (x, ncopy, TY_BOOL)
	    while (ncopy > 0) {
		if (tbagtb (itp, icp, row, Memb[x], first, ncopy) < ncopy)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbcptb (otp, ocp, Memb[x], first, last)
		call taex_incr (nremain, ncopy, first, last, BUFSIZE)
	    }

	} else if (datatype < 0) {		# character string
	    slen = -datatype + 3		# a little extra space
	    call salloc (x, slen, TY_CHAR)
	    do i = 1, nelem {
		if (tbagtt (itp, icp, row, Memc[x], slen, i, 1) < 1)
		    call taex_disaster (itp, otp, delete_flag,
				"error reading input")
		call tbeptt (otp, ocp, i, Memc[x])
	    }

	} else {
	    call taex_disaster (itp, otp, delete_flag, "unknown data type")
	}

	# If we wrote to an existing column in an existing table, and the
	# output table has more rows than we just wrote, then we should set
	# the remaining rows in this column to INDEF.
	if (!newcolumn) {
	    do i = nelem+1, tbpsta (otp, TBL_NROWS)
		call tbrudf (otp, ocp, 1, i)
	}

	call tbtclo (otp)
	call tbtclo (itp)

	call sfree (sp)
end

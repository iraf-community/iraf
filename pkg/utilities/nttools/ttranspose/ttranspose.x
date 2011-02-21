include <error.h>	# for EA_WARN
include <fset.h>	# to check whether input or output is redirected
include <tbset.h>

# ttranspose -- transpose or flip a table
# This task can be used to transpose a table so that input rows become
# output columns and input columns become output rows.  Another option
# is to flip the table horizontally, that is, the first input column is
# the last output column.  Finally, the table can be flipped vertically,
# i.e., the first input row is the last output row.  Any combination of
# these operations may be performed.
#
# Phil Hodge, 30-Nov-1994  Task created.
# Phil Hodge,  4-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge, 12-Jun-1998  Initialize maxwidth using tbcftl in ttr_compare.
# Phil Hodge,  8-Apr-1999  Call tbfpri.
# Phil Hodge,  8-Jun-1999  Set input/output to STDIN/STDOUT if redirected.

procedure ttranspose()

pointer innames		# scratch for names of input tables
pointer outnames	# scratch for names of output tables
pointer action		# scratch for operations to perform
bool	verbose		# print table names?
#--
pointer sp
pointer ilist, olist	# tbn pointers for input & output lists
pointer intable		# scratch for name of an input table
pointer outtable	# scratch for name of an output table
pointer itp, otp	# pointers to input & output table structs
pointer icp, ocp	# pointers to arrays of column descriptors
int	op[2]		# mapping of (columns,rows) from input to output
int	dtype		# data type of columns
int	nelem		# number of elements in array
int	irows, orows	# number of rows in input, output tables
int	icols, ocols	# number of columns in input, output tables
int	i		# loop index
int	junk
int	num_out		# number of output table names
int	phu_copied	# set by tbfpri and ignored
bool	transpose	# true if table will be transposed, not just flipped
bool	to_stdout	# true if output is to STDOUT
bool	clgetb(), streq()
int	fstati()
pointer tbnopen()
int	tbnlen(), tbnget()
pointer tbtopn(), tbcnum()
int	tbpsta()
errchk	ttr_compare, tbfpri, ttr_create, ttr_trans, ttr_flip
string	SKIP	"Table %s will be skipped:\n"

begin
	call smark (sp)
	call salloc (innames, SZ_LINE, TY_CHAR)
	call salloc (outnames, SZ_LINE, TY_CHAR)
	call salloc (intable, SZ_LINE, TY_CHAR)
	call salloc (outtable, SZ_LINE, TY_CHAR)
	call salloc (action, SZ_LINE, TY_CHAR)

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", Memc[innames], SZ_LINE)
	else
	    call clgstr ("intable", Memc[innames], SZ_LINE)

	if (fstati (STDOUT, F_REDIR) == YES)
	    call strcpy ("STDOUT", Memc[outnames], SZ_LINE)
	else
	    call clgstr ("outtable", Memc[outnames], SZ_LINE)

	call clgstr ("action", Memc[action], SZ_LINE)
	verbose = clgetb ("verbose")

	# Interpret the list of operations.
	call ttr_opcode (Memc[action], op, transpose)

	ilist = tbnopen (Memc[innames])
	olist = tbnopen (Memc[outnames])

	to_stdout = false			# may be updated below
	num_out = tbnlen (olist)
	# Get the first output name now, and then rewind the list.
	junk = tbnget (olist, Memc[outtable], SZ_LINE)
	call tbnrew (olist)

	if (tbnlen (ilist) != num_out) {
	    if (num_out == 1 && streq (Memc[outtable], "STDOUT")) {
		# It's OK to have multiple input tables and just one output
		# if the latter is STDOUT.
		to_stdout = true
	    } else {
		call tbnclose (olist)
		call tbnclose (ilist)
		call error (1, "Input and output lists not the same length.")
	    }
	}

	# Do for each table in the list.
	while (tbnget (ilist, Memc[intable], SZ_LINE) != EOF) {
	    if (num_out > 1)
		junk = tbnget (olist, Memc[outtable], SZ_LINE)

	    # Open input table and get number of rows and columns.
	    itp = tbtopn (Memc[intable], READ_ONLY, NULL)
	    irows = tbpsta (itp, TBL_NROWS)
	    icols = tbpsta (itp, TBL_NCOLS)
	    call tbtnam (itp, Memc[intable], SZ_LINE)	# get full table name

	    # Allocate space for pointers to column descriptors.
	    call malloc (icp, icols, TY_POINTER)

	    # Get column pointers for input table.
	    do i = 1, icols
		Memi[icp+i-1] = tbcnum (itp, i)

	    iferr {
		# Check that data types of columns are all the same.
		call ttr_compare (itp, Memi[icp], icols,
			transpose, dtype, nelem)

		# Create output table.
		call tbfpri (Memc[intable], Memc[outtable], phu_copied)
		call ttr_create (itp, otp, Memi[icp], ocp, Memc[outtable],
			op, transpose, irows, icols, orows, ocols, dtype, nelem)
	    } then {
		call mfree (icp, TY_POINTER)
		call tbtclo (itp)
		call eprintf (SKIP)
		    call pargstr (Memc[intable])
		call erract (EA_WARN)
		next
	    }

	    if (verbose) {
		call printf ("%s --> %s\n")
		    call pargstr (Memc[intable])
		    call pargstr (Memc[outtable])
		call flush (STDOUT)
	    }

	    # Copy table data.
	    iferr {
		if (transpose) {
		    call ttr_trans (itp, otp, Memi[icp], Memi[ocp],
			irows, icols, orows, ocols, op, dtype, nelem)
		} else {
		    call ttr_flip (itp, otp, Memi[icp], Memi[ocp],
			irows, icols, op)
		}
	    } then {
		call mfree (ocp, TY_POINTER)
		call mfree (icp, TY_POINTER)
		call tbtclo (otp)
		call tbtclo (itp)
		call tbtdel (Memc[outtable])
		call eprintf (SKIP)
		    call pargstr (Memc[intable])
		call erract (EA_WARN)
		next
	    }

	    call mfree (ocp, TY_POINTER)
	    call mfree (icp, TY_POINTER)
	    iferr {
		call tbtclo (otp)
	    } then {
		call eprintf (SKIP)
		    call pargstr (Memc[intable])
		call erract (EA_WARN)
	    }
	    call tbtclo (itp)
	}

	call tbnclose (olist)
	call tbnclose (ilist)
	call sfree (sp)
end

procedure ttr_opcode (action, op, transpose)

char	action[ARB]	# i: list of operations to perform
int	op[2]		# o: combined operations
bool	transpose	# o: true if table will be transposed, not just flipped
#--
int	i
int	prev[2]		# previous op
int	slen		# length of string
int	strlen()

begin
	slen = strlen (action)

	prev[1] = 1				# initial values
	prev[2] = 2

	do i = 1, slen {
	    if (action[i] == 't' || action[i] == '/') {
		# transpose
		op[1] = prev[2]
		op[2] = prev[1]
	    } else if (action[i] == 'h' || action[i] == '-') {
		# flip horizontally, i.e. first column <--> last col
		op[1] = -prev[1]
		op[2] =  prev[2]
	    } else if (action[i] == 'v' || action[i] == '|') {
		# flip vertically, i.e. first row <--> last row
		op[1] =  prev[1]
		op[2] = -prev[2]
	    } else if (action[i] == ',' || action[i] == ' ') {
		;
	    } else {
		call error (1, "'action' must use only t, h, v")
	    }
	    prev[1] = op[1]			# save for next loop
	    prev[2] = op[2]
	}

	# After all the operations, will we actually transpose the table,
	# or just flip it?
	transpose = (abs (op[1]) == 2)
end

# ttr_compare -- compare data types and array lengths

procedure ttr_compare (itp, icp, icols, transpose, dtype, nelem)

pointer itp		# i: pointer to input table struct
pointer icp[icols]	# i: array of pointers to input column descriptors
int	icols		# i: number of columns in input table
bool	transpose	# i: true if table will be transposed, not just flipped
int	dtype		# o: data type of columns
int	nelem		# o: length of array stored at each row,column
#--
int	dtype2, nelem2	# data type and array length of column to compare
int	width		# width of a particular column
int	maxwidth	# max width of column in text table
int	i
int	tbpsta(), tbcigi()

begin
	# Get info about first column so we can compare with other columns.
	dtype = tbcigi (icp[1], TBL_COL_DATATYPE)
	nelem = tbcigi (icp[1], TBL_COL_LENDATA)
	if (dtype == TY_CHAR) {			# old style, change it
	    dtype = -nelem
	    nelem = 1
	}

	# We don't need to check column data types if we're not actually
	# transposing the table.
	if (!transpose)
	    return

	if (tbpsta (itp, TBL_WHTYPE) == TBL_TYPE_TEXT) {

	    # For a text table, we can permit different input data types
	    # if we set the output type to text.

	    call tbcftl (icp[1], maxwidth)	# maxwidth updated in loop
	    do i = 2, icols {

		call tbcftl (icp[i], width)	# get width of current column
		maxwidth = max (maxwidth, width)

		dtype2 = tbcigi (icp[i], TBL_COL_DATATYPE)
		if (dtype > 0 && dtype != dtype2) {
		    # They're not the same; change to char data type.
		    dtype = -maxwidth
		}
	    }
	    if (dtype < 0)
		dtype = -maxwidth

	} else {				# not a text table

	    # Compare first column with subsequent columns.
	    do i = 2, icols {

		dtype2 = tbcigi (icp[i], TBL_COL_DATATYPE)
		nelem2 = tbcigi (icp[i], TBL_COL_LENDATA)
		if (dtype2 == TY_CHAR) {
		    dtype2 = -nelem2
		    nelem2 = 1
		}

		if (dtype < 0) {
		    # For character columns, allow different lengths for
		    # input, but change to maximum length for output.
		    dtype = min (dtype, dtype2)	# max absolute value

		} else if (dtype != dtype2) {

		    # Promote real to double, short to int, and bool to
		    # any other type.
		    if (dtype == TY_REAL && dtype2 == TY_DOUBLE ||
			dtype == TY_DOUBLE && dtype2 == TY_REAL) {
			dtype = TY_DOUBLE
		    } else if (dtype == TY_INT && dtype2 == TY_SHORT ||
			dtype == TY_SHORT && dtype2 == TY_INT) {
			dtype = TY_INT
		    } else if (dtype == TY_BOOL) {
			dtype = dtype2			# promote to other type
		    } else if (dtype2 == TY_BOOL) {
			;			# OK to convert to any type
		    } else {
			call error (1, "columns are not all the same data type")
		    }
		}

		if (nelem != nelem2)
		    call error (1, "column array lengths are not all the same")
	    }
	}
end

# ttr_create -- create output table
# This routine creates the output table, defines output columns,
# and copies header parameters.
#
# Note the following, which can be a bit confusing.  In the case that
# the table is to be flipped horizontally but not transposed, columns are
# defined in the output table in the reverse order from the corresponding
# columns in the input table, but the column pointers themselves are stored
# in their arrays in the same order.  That is, icp[i] refers to the same
# column as Memi[ocp+i-1], except of course that icp[i] is in the input
# table and Memi[ocp+i-1] is in the output table.  "Same column" means
# that the column descriptions and contents are the same, but the column
# number will in general be different; icp[1] is the first column in the
# input table, Memi[ocp] is the last column in the output table, and they
# will have the same name, etc.

procedure ttr_create (itp, otp, icp, ocp, outtable,
		op, transpose, irows, icols, orows, ocols, dtype, nelem)

pointer itp		# i: pointer to input table struct
pointer otp		# o: pointer to output table struct
pointer icp[icols]	# i: array of pointers to input column descriptors
pointer ocp		# o: pointer to array of pointers to output col descr
char	outtable[ARB]	# io: name of output table (extension may be appended)
int	op[2]		# i: operation code
bool	transpose	# i: true if table will be transposed, not just flipped
int	irows		# i: number of rows in input table
int	icols		# i: number of columns in input table
int	orows		# o: number of rows in output table
int	ocols		# o: number of columns in output table
int	dtype		# i: data type of columns
int	nelem		# i: length of array stored at each row,column
#--
char	colname[SZ_COLNAME]	# name of current column
char	colunits[SZ_COLUNITS]	# units for current column
char	colfmt[SZ_COLFMT]	# print format for current column
int	datatype		# data type of current column
int	lenarray		# number of elements for current column
int	lenfmt			# space required to print column
int	colnum			# column number
int	i			# loop index
int	i_start, i_end, i_incr	# loop limits for index i
int	maxpar			# space allocated for header parameters
pointer tbtopn()
int	tbpsta()

begin
	# Allocate space for array of column pointers for output table.
	if (transpose) {
	    orows = icols
	    ocols = irows
	} else {				# don't transpose
	    orows = irows
	    ocols = icols
	}
	call malloc (ocp, ocols, TY_POINTER)

	# Create output table.
	otp = tbtopn (outtable, NEW_FILE, NULL)
	if (tbpsta (itp, TBL_WHTYPE) == TBL_TYPE_TEXT)
	    call tbpset (otp, TBL_WHTYPE, TBL_TYPE_TEXT)

	# Set enough space for all header parameters from input.
	maxpar = tbpsta (itp, TBL_MAXPAR)
	call tbpset (otp, TBL_MAXPAR, maxpar)

	# Create output columns.
	if (transpose) {

	    # Assign dummy column names, with null units and default format.
	    do i = 1, ocols {
		call sprintf (colname, SZ_COLNAME, "c%d")
		    call pargi (i)
		call tbcdef (otp, Memi[ocp+i-1],
			colname, "", "", dtype, nelem, 1)
	    }

	} else {

	    # We're not transposing, so retain column names, etc.
	    if (op[1] > 0) {
		# retain order of columns
		i_start = 1
		i_end = icols
		i_incr = 1
	    } else {
		# flip by defining last column first
		i_start = icols
		i_end = 1
		i_incr = -1
	    }

	    do i = i_start, i_end, i_incr {
		call tbcinf (icp[i], colnum, colname, colunits, colfmt,
			datatype, lenarray, lenfmt)
		call tbcdef (otp, Memi[ocp+i-1],
			colname, colunits, colfmt, datatype, lenarray, 1)
	    }
	}
	call tbtcre (otp)
	call tbtnam (otp, outtable, SZ_LINE)	# get full table name

	# Copy all header parameters from input to output.
	call tbhcal (itp, otp)
end

include <tbset.h>

# The values of the independent variable for the output table can be
# read either from an input table (xtable), or they can be assigned
# from the start, increment, and end values.
#
# Phil Hodge, 25-Apr-2000  Subroutine created.
# Phil Hodge, 12-May-2004  errchk the procedures that are called.

procedure tuxget (xtable, iv_start, iv_end, iv_step, padvalue,
		xout, outrows)

char	xtable[ARB]	# i: table of output indep var values
double	iv_start	# i: starting value of independent variable
double	iv_end		# i: ending value of independent variable
double	iv_step		# i: increment in independent variable
double	padvalue	# i: value at end of indep var array to ignore
pointer xout		# o: pointer to output indep var values
int	outrows		# o: number of output rows (size of xout)
#--
pointer tp, cp
int	row, nrows
int	i
int	nelem		# array size, or number of table rows
int	nvals		# number of elements read from column if it's an array
double	dbuf
double	direction	# indicates increasing or decreasing data
double	previous	# for comparing current and previous values
pointer tbtopn(), tbcnum()
int	tbpsta(), tbcigi(), tbagtd()
errchk	tbtopn, tbpsta, tbcnum, tbcigi, tbtclo, tbagtd, tbegtd, tu_trim

begin
	if (xtable[1] != EOS) {

	    tp = tbtopn (xtable, READ_ONLY, NULL)
	    if (tbpsta (tp, TBL_NCOLS) < 1) {
		call tbtclo (tp)
		call error (1, "No data in xtable")
	    }
	    if (tbpsta (tp, TBL_NCOLS) > 1) {
		call tbtclo (tp)
		call eprintf ("xtable %s contains more than one column;\n")
		    call pargstr (xtable)
		call eprintf (
	"use a column selector [c:<colname>] to specify which column.\n")
		call error (1, "")
	    }

	    nrows = tbpsta (tp, TBL_NROWS)
	    cp = tbcnum (tp, 1)
	    nelem = tbcigi (cp, TBL_COL_LENDATA)

	    if (nelem > 1 && nrows > 1) {
		call tbtclo (tp)
		call eprintf ("xtable %s contains more than one row,\n")
		    call pargstr (xtable)
		call eprintf ("and the column contains arrays;\n")
		call eprintf (
	"use a row selector [c:row=<rownum>] to specify which row.\n")
		call error (1, "")
	    }

	    if (nelem == 1)
		nelem = nrows
	    call malloc (xout, nelem, TY_DOUBLE)

	    # Read the data from the table.
	    if (nrows == 1) {
		row = 1
		nvals = tbagtd (tp, cp, row, Memd[xout], 1, nelem)
		if (nvals < nelem) {
		    call tbtclo (tp)
		    call error (1, "not all elements read from xtable")
		}
	    } else {
		do row = 1, nrows
		    call tbegtd (tp, cp, row, Memd[xout+row-1])
	    }

	    call tbtclo (tp)

	    # Trim trailing garbage by decrementing outrows.
	    outrows = nelem
	    call tu_trim (Memd[xout], outrows, padvalue)

	    # Check for embedded INDEF values, and make sure the values
	    # are monotonically increasing or decreasing.
	    if (outrows > 1) {

		do i = 1, outrows {
		    if (IS_INDEFD(Memd[xout+i-1])) {
			call eprintf (
			"xtable %s contains embedded INDEF values\n")
			    call pargstr (xtable)
			call eprintf ("(i.e. not just trailing INDEFs)\n")
			call error (1, "")
		    }
		}

		if (Memd[xout+1] >= Memd[xout])
		    direction = 1.d0
		else
		    direction = -1.d0
		previous = Memd[xout] - direction
		do i = 1, outrows {
		    if (direction * (Memd[xout+i-1] - previous) <= 0.d0) {
			call eprintf (
			"Values in xtable %s are not monotonic\n")
			    call pargstr (xtable)
			call error (1, "")
		    }
		    previous = Memd[xout+i-1]
		}
	    }

	} else {			# no xtable

	    # Find out how many rows the output table should have.
	    if (iv_start == iv_end)
		outrows = 1
	    else
		outrows = nint ((iv_end - iv_start) / iv_step + 1.0)

	    call malloc (xout, outrows, TY_DOUBLE)

	    # Compute the independent variable values for the output table.
	    dbuf = iv_start
	    do i = 1, outrows {
		Memd[xout+i-1] = dbuf
		dbuf = dbuf + iv_step
	    }
	}
end

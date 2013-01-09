include "tbtables.h"
include "tblerr.h"

# tbzsft -- Z shift rows
# Shift one or more rows down (to leave a gap in the table) or up (to
# delete rows).  The range of rows that is shifted is from FIRST to
# the last row in the table.  Shift down if SHIFT > 0, or shift up if
# SHIFT < 0.  SHIFT is the number of rows by which to shift.
#
# If SHIFT > 0 rows that are exposed by the shift are NOT set to indef.
# If SHIFT < 0 rows at the end WILL be set to indef.
# In either case the number of rows TB_NROWS(tp) will be updated.
#
# Phil Hodge, 31-Jan-1992  Subroutine created.

procedure tbzsft (tp, first, shift)

pointer tp			# i: pointer to table descriptor
int	first			# i: first row to be affected by the shift
int	shift			# i: shift by this many rows
#--
pointer cptr			# pointer to a column descriptor
pointer v			# pointer to array of values
int	abs_shift		# absolute value of shift
int	row1, row2		# range of rows to be copied
int	nrows			# number of rows written to table
int	dtype			# data type of a column
int	col			# loop index for column number
int	ip, op			# loop indexes
pointer tbcnum()

begin
	nrows = TB_NROWS(tp)

	# Make sure there are enough rows allocated in the table.
	if (first > nrows) {
	    if (shift > 0) {
		row2 = shift + first - 1
		if (row2 > TB_ALLROWS(tp)) {
		    call tbtchs (tp, -1, -1, -1, row2)
		}
	    }
	    return					# nothing else to do
	} else {
	    row2 = shift + nrows
	    if (row2 > TB_ALLROWS(tp)) {
		call tbtchs (tp, -1, -1, -1, row2)
	    }
	}

	if (shift == 0)
	    return

	# Consider the case of deleting all rows starting with FIRST.
	if (nrows + shift < first) {
	    call tbznll (tp, first, nrows)
	    TB_NROWS(tp) = max (0, first-1)
	    return
	}

	if (shift > 0)
	    TB_NROWS(tp) = TB_NROWS(tp) + shift

	abs_shift = abs (shift)

	# Rows row1:row2 will be copied to row1+shift:row2+shift.
	if (shift < 0)
	    row1 = first + abs_shift
	else
	    row1 = first
	row2 = nrows

	do col = 1, TB_NCOLS(tp) {

	    cptr = tbcnum (tp, col)
	    dtype = COL_DTYPE(cptr)
	    v = COL_OFFSET(cptr)		# pointer to column values

	    if (dtype == TBL_TY_DOUBLE) {

		if (shift < 0) {
		    op = first
		    do ip = row1, row2 {
			Memd[v+op-1] = Memd[v+ip-1]
			op = op + 1
		    }

		} else {			# shift > 0
		    op = nrows + shift
		    do ip = row2, row1, -1 {
			Memd[v+op-1] = Memd[v+ip-1]
			op = op - 1
		    }
		}

	    } else if (dtype == TBL_TY_INT) {

		if (shift < 0) {
		    op = first
		    do ip = row1, row2 {
			Memi[v+op-1] = Memi[v+ip-1]
			op = op + 1
		    }

		} else {			# shift > 0
		    op = nrows + shift
		    do ip = row2, row1, -1 {
			Memi[v+op-1] = Memi[v+ip-1]
			op = op - 1
		    }
		}

	    } else if (dtype < 0 || dtype == TBL_TY_CHAR) {

		call malloc (v, SZ_LINE, TY_CHAR)
		if (shift < 0) {
		    op = first
		    do ip = row1, row2 {
			call tbegtt (tp, cptr, ip, Memc[v], SZ_LINE)
			call tbeptt (tp, cptr, op, Memc[v])
			op = op + 1
		    }

		} else {			# shift > 0
		    op = nrows + shift
		    do ip = row2, row1, -1 {
			call tbegtt (tp, cptr, ip, Memc[v], SZ_LINE)
			call tbeptt (tp, cptr, op, Memc[v])
			op = op - 1
		    }
		}
		call mfree (v, TY_CHAR)

	    } else {
		call error (ER_TBCOLBADTYP,
			"tbzsft:  table or memory corrupted?")
	    }
	}

	# If rows were deleted, set the extra rows at end to indef,
	# and change the value of TB_NROWS(tp).
	if (shift < 0) {
	    call tbznll (tp, nrows-abs_shift+1, nrows)
	    TB_NROWS(tp) = max (0, nrows - abs_shift)
	}
end

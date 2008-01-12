include "tbtables.h"

# tbfsft -- shift rows in a FITS table
# Shift one or more rows down (to leave a gap in the table) or up (to
# delete rows).  The range of rows that is shifted is from FIRST to
# the last row in the table.  Shift down if SHIFT > 0, or shift up if
# SHIFT < 0.  SHIFT is the number of rows by which to shift.
#
# Rows that are exposed by the shift are NOT set to indef.  The total
# number of rows TB_NROWS(tp) will be reduced if SHIFT < 0, and it will
# be increased if SHIFT > 0.
#
# NOTE:  This routine must not be used if a row selector is in effect.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  6-Mar-1998  Error check tbferr instead of tbfnll.

procedure tbfsft (tp, first, shift)

pointer tp		# i: pointer to table descriptor
int	first		# i: first row to be moved
int	shift		# i: shift by this many rows
#--
int	abs_shift	# absolute value of shift
int	row1		# first row of a range to be copied
int	nrows		# number of rows written to table
int	j, k		# loop indexes
int	status
errchk	tbrcpy, tbferr

begin
	nrows = TB_NROWS(tp)
	abs_shift = abs (shift)

	if (first > nrows)
	    return

	if (shift < 0) {

	    # Shift to smaller rows, overwriting rows starting with FIRST.
	    k = first
	    do j = first + abs_shift, nrows {
		call tbrcpy (tp, tp, j, k)	# copy row j to row k
		k = k + 1
	    }
	    # Delete rows at end.
	    status = 0
	    call fsdrow (TB_FILE(tp), nrows-abs_shift+1, abs_shift, status)
	    if (status > 0)
		call tbferr (status)

	    # Change the value of TB_NROWS.
	    TB_NROWS(tp) = max (0, nrows - abs_shift)

	} else {				# shift down

	    row1 = nrows - shift + 1

	    if (row1 >= first) {

		# First copy the block of rows that are to be put beyond
		# the current EOF; with each call in this loop we are
		# writing the next row beyond EOF.
		k = nrows + 1
		do j = row1, nrows {
		    call tbrcpy (tp, tp, j, k)
		    k = k + 1
		}

		k = nrows
		do j = nrows - shift, first, -1 {
		    call tbrcpy (tp, tp, j, k)
		    k = k - 1
		}
	    } else {
		# The entire block is to be shifted beyond current EOF.
		k = first + shift
		do j = first, nrows {
		    call tbrcpy (tp, tp, j, k)
		    k = k + 1
		}
	    }
	}
end

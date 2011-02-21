include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrsft -- shift rows
# Shift one or more rows down (to leave a gap in the table) or up (to
# delete rows).  The range of rows that is shifted is from FIRST to
# the last row in the table.  Shift down if SHIFT > 0, or shift up if
# SHIFT < 0.  SHIFT is the number of rows by which to shift.
#
# Rows that are exposed by the shift are NOT set to indef.  The total
# number of rows TB_NROWS(tp) will NOT be reduced if SHIFT < 0, but
# it will be increased if SHIFT > 0.  The calling routine (e.g. tbrdel)
# is responsible for cleaning up such details.
#
# Phil Hodge, 23-Mar-1988  Subroutine created.
# Phil Hodge, 30-Jan-1992  Add option for text table type.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 21-Jun-1995  Modify for FITS tables.
# Phil Hodge,  3-Mar-1998  Error if a row selector is in effect.

procedure tbrsft (tp, first, shift)

pointer tp		# i: pointer to table descriptor
int	first		# i: first row to be moved
int	shift		# i: shift by this many rows
#--
errchk	tbxsft, tbysft, tbzsft, tbfsft

begin
	if (shift == 0)
	    return

	if (TB_ROW_SELECT(tp) == YES) {
	    call error (1,
		"Can't shift rows in a table with row selector in effect.")
	}

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxsft (tp, first, shift)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbysft (tp, first, shift)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    call tbzsft (tp, first, shift)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    call tbfsft (tp, first, shift)
	else
	    call error (ER_TBCORRUPTED, "tbrsft:  table type is messed up")

	TB_MODIFIED(tp) = true
end

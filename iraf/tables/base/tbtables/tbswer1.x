include <tbset.h>
include "tbtables.h"

# tbswer1 -- write empty rows, maybe

# This routine is like tbswer with one significant difference.  If the row
# to be written (selrow) is equal to the current number of rows plus one,
# and if the table is an stsdas type row-ordered table, then this routine
# does NOT write an INDEF row to the table and does NOT update TB_NROWS.
# This is to improve efficiency for this common case, so that the row
# does not need to be written twice (once INDEF, then the actual data).
# Routines such as tbxap[] may take advantage of this option.
#
# For other cases, this routine just calls tbswer, which does write INDEF
# rows to the table and does update TB_NROWS.
#
# If a row selector is in effect, selrow will be converted to rownum, and
# new rows (if rownum is beyond EOF) will be added to the set of selected
# rows.  If there is no row selector, the value of selrow will be assigned
# directly to rownum.
#
# Phil Hodge,  4-Mar-1998  Subroutine created.

procedure tbswer1 (tp, selrow, rownum)

pointer tp		# i: pointer to table descriptor
int	selrow		# i: row number (or selected row number)
int	rownum		# o: actual row number
#--
errchk	tbsrow, tbswer

begin
	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {

	    if (TB_ROW_SELECT(tp) == YES) {

		if (selrow > TB_NSEL_ROWS(tp) + 1)
		    call tbswer (tp, selrow, rownum)
		else
		    call tbsrow (tp, selrow, rownum)	# TB_NROWS not updated

	    } else {

		rownum = selrow
		if (rownum > TB_NROWS(tp) + 1)
		    call tbswer (tp, selrow, rownum)
	    }

	} else {

	    call tbswer (tp, selrow, rownum)
	}
end

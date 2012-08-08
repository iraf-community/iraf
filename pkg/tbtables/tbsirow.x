include "tbtables.h"
include "tblerr.h"

# tbsirow -- get actual row number from selected row number
#
# This routine is for translating the selected row number to the actual
# row number for an input table, i.e. for the case where the row number
# may not be greater than the number of rows in the table.

procedure tbsirow (tp, selrow, rownum)

pointer tp		# i: pointer to table descriptor
int	selrow		# i: row number (or selected row number)
int	rownum		# o: actual row number
#--
int	rst_rownum()

begin
	if (selrow < 1)
	    call error (1, "row number less than one is invalid")

	if (TB_ROW_SELECT(tp) == YES) {

	    if (selrow > TB_NSEL_ROWS(tp))
		call error (ER_TBBEYONDEOF, "input row is beyond EOF")

	    rownum = rst_rownum (TB_ROWSET(tp), selrow)

	} else {

	    if (selrow > TB_NROWS(tp))
		call error (ER_TBBEYONDEOF, "input row is beyond EOF")

	    rownum = selrow
	}
end

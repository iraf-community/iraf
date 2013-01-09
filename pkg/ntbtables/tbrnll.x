include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrnll -- set rows to null
# This procedure sets all columns in a range of rows to INDEF.
# If the first row to be deleted is greater than the last row, or if
# the range of rows is outside the allocated size of the table, nothing
# is done.  It is not considered an error if the first row is less than
# one or the last row is greater than the number of allocated rows in
# the table.
#
# Phil Hodge,  7-Mar-1988  Subroutine created.
# Phil Hodge,  3-Feb-1992  Add option for text table type.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 21-Jun-1995  Modify for FITS tables.
# Phil Hodge,  3-Mar-1998  Modify to allow for row selector.

procedure tbrnll (tp, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
int	sel_firstrow	# i: first row to be set to INDEF
int	sel_lastrow	# i: last row to be set to INDEF
#--
int	firstrow, lastrow	# range of actual row numbers
int	rst_rownum()
errchk	rst_rownum, tbxnll, tbynll, tbznll, tbfnll

begin
	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbrnll:  lastrow is less than firstrow")

	if (TB_ROW_SELECT(tp) == YES) {

	    if (sel_firstrow > TB_NSEL_ROWS(tp))
		return					# nothing to do
	    firstrow = rst_rownum (TB_ROWSET(tp), sel_firstrow)

	    if (sel_lastrow > TB_NSEL_ROWS(tp))
		lastrow = TB_NROWS(tp)			# stop at last row
	    else
		lastrow = rst_rownum (TB_ROWSET(tp), sel_lastrow)

	} else {

	    firstrow = sel_firstrow
	    if (firstrow > TB_NROWS(tp))
		return

	    lastrow = sel_lastrow
	    if (lastrow > TB_NROWS(tp))
		lastrow = TB_NROWS(tp)
	}

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxnll (tp, firstrow, lastrow)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbynll (tp, firstrow, lastrow)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    call tbznll (tp, firstrow, lastrow)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    call tbfnll (tp, firstrow, lastrow)
	else
	    call error (ER_TBCORRUPTED, "tbrnll:  table type is messed up")

	TB_MODIFIED(tp) = true
end

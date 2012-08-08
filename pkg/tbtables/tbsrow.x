include "tbtables.h"

# tbsrow -- add new rows to row select list
#
# The purpose of this routine is to translate the selected row number to
# an actual row number for a table opened read-write.  This would not
# normally be called by itself; use tbswer instead, which itself calls
# this routine.
#
# For the case that selrow is larger than the last currently selected row,
# this routine adds one or more new rows to the list of selected rows, with
# the new row numbers being larger than the current number of rows in the
# table.  The number of new rows is selrow minus the current number of
# selected rows.  In this case the calling routine should actually write
# rows to the table, possibly just INDEF.
#
# The table itself will not be modified by this routine.
#
# NOTE that if rownum is greater than the current number of rows in the
# table, it is important to write to the extra rows and update TB_NROWS
# before calling any other high-level routines.
#
# Phil Hodge,  3-Mar-1998  Subroutine created.

procedure tbsrow (tp, selrow, rownum)

pointer tp		# i: pointer to table descriptor
int	selrow		# i: row number (or selected row number)
int	rownum		# o: actual row number
#--
int	num_new_rows		# number of new rows to add
int	rst_rownum(), rst_nelem()
errchk	rst_rownum, rst_addtab

begin
	if (selrow < 1)
	    call error (1, "row number less than one is invalid")

	if (TB_ROW_SELECT(tp) == YES) {

	    if (selrow > TB_NSEL_ROWS(tp)) {

		num_new_rows = selrow - TB_NSEL_ROWS(tp)
		rownum = TB_NROWS(tp) + num_new_rows

		# Include the new row(s) in the list of "selected" rows.
		call rst_addtab (TB_ROWSET(tp), TB_NROWS(tp), num_new_rows)

		# Update the number of selected rows.
		TB_NSEL_ROWS(tp) = rst_nelem (TB_ROWSET(tp))

	    } else {

		rownum = rst_rownum (TB_ROWSET(tp), selrow)
	    }

	} else {

	    rownum = selrow
	}
end

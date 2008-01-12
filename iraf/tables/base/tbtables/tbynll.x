include "tbtables.h"

# tbynll -- Y set rows to null
# This procedure sets all columns in a range of rows to INDEF.
# If the first row to be deleted is greater than the last row, or if
# the range of rows is outside the allocated size of the table, nothing
# is done.  It is not considered an error if the first row is less than
# one or the last row is greater than the number of allocated rows in
# the table.
#
# Phil Hodge,  7-Mar-1988  Subroutine created.

procedure tbynll (tp, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
int	firstrow		# i: first row to be set to INDEF
int	lastrow			# i: last row to be set to INDEF
#--
pointer sp
pointer colptr			# scratch for array of column pointers
int	row1, row2		# firstrow, lastrow truncated to 1, nrows
int	k			# loop index for column number
pointer tbcnum()
errchk	tbyscn

begin
	row1 = max (1, firstrow)
	row2 = min (TB_ALLROWS(tp), lastrow)

	if (row1 > row2)
	    return

	call smark (sp)
	call salloc (colptr, TB_NCOLS(tp), TY_INT)
	do k = 1, TB_NCOLS(tp)
	    Memi[colptr+k-1] = tbcnum (tp, k)
	call tbyscn (tp, TB_FILE(tp), Memi[colptr], TB_NCOLS(tp), row1, row2)
	call sfree (sp)
end

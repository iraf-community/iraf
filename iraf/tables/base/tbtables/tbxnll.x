include "tbtables.h"

# tbxnll -- X set rows to null
# This procedure sets all columns in a range of rows to INDEF.
# If the first row to be deleted is greater than the last row, or if
# the range of rows is outside the allocated size of the table, nothing
# is done.  It is not considered an error if the first row is less than
# one or the last row is greater than the number of allocated rows in
# the table.
#
# Phil Hodge,  7-Mar-1988  Subroutine created.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.

procedure tbxnll (tp, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
long	firstrow		# i: first row to be set to INDEF
long	lastrow			# i: last row to be set to INDEF
#--
long	row1, row2		# firstrow, lastrow truncated to 1, nrows
long	row			# loop index for row number
size_t	sz_val
long	tbxoff()		# function returning offset to beginning of row
errchk	seek, write

begin
	row1 = max (1, firstrow)
	row2 = min (TB_NROWS(tp), lastrow)

	# Write the indef record at each row to be set to INDEF.
	do row = row1, row2 {
	    call seek (TB_FILE(tp), tbxoff (tp, row))
	    sz_val = TB_ROWLEN(tp)
	    call write (TB_FILE(tp), Memc[TB_INDEF(tp)], sz_val)
	}
end

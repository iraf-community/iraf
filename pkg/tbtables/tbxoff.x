include "tbtables.h"

# tbxoff -- X row offset
# This function returns the offset in char from the beginning of the
# table data file to the beginning of a row in the table.
# Note that the offset is to the beginning, not to a specific column.
# This is for row-ordered tables.

long procedure tbxoff (tp, rownum)

pointer tp			# Pointer to table descriptor
int	rownum			# The row number

long	offset			# offset to beginning of row

begin
	offset = TB_BOD(tp) + (rownum - 1) * TB_ROWLEN(tp)
	return (offset)
end

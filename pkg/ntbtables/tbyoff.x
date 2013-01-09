include "tbtables.h"

# tbyoff -- Y column offset
# This function returns the offset in char from the beginning of the
# table data file to a specific element in the table.
# This is for column-ordered tables.

long procedure tbyoff (tp, cp, rownum)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: the row number

long	offset

begin
	offset = TB_BOD(tp) + COL_OFFSET(cp) * TB_ALLROWS(tp) +
		(rownum-1) * COL_LEN(cp)
	return (offset)
end

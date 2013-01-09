include "tbtables.h"

# tbyncn -- Y new column null
# Write INDEF values for each new column in each row of a table.
# This is called after defining new columns in an open table.

procedure tbyncn (tp, colptr, numcols)

pointer tp			# Pointer to table descriptor
pointer colptr[numcols]		# Array of pointers to descr of new columns
int	numcols			# The number of new columns

int	fd			# identifies the file for the table data
int	firstrow		# the first row to be set to indef
int	lastrow			# the last row to be set to indef
errchk	tbyscn

begin
	firstrow = 1
	lastrow = TB_ALLROWS(tp)
	fd = TB_FILE(tp)
	# set columns to null
	call tbyscn (tp, fd, colptr, numcols, firstrow, lastrow)
end

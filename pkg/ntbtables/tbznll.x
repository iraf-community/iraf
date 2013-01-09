include "tbtables.h"
include "tblerr.h"

# tbznll -- set rows to null
# This procedure sets all columns in a range of rows to INDEF.
# If the first row to be deleted is greater than the last row, or if
# the range of rows is outside the allocated size of the table, nothing
# is done.  It is not considered an error if the first row is less than
# one or the last row is greater than the number of allocated rows in
# the table.
#
# Phil Hodge,  3-Feb-1992  Subroutine created.

procedure tbznll (tp, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
int	firstrow		# i: first row to be set to INDEF
int	lastrow			# i: last row to be set to INDEF
#--
pointer cp			# pointer to a column descriptor
int	row1, row2		# firstrow, lastrow truncated to 1, nrows
int	row			# loop index for row number
int	col			# loop index for column number
int	datatype		# data type of a column
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc
pointer tbcnum()

begin
	row1 = max (1, firstrow)
	row2 = min (TB_ALLROWS(tp), lastrow)

	# Set each column value in each row to INDEF.
	do col = 1, TB_NCOLS(tp) {
	    cp = tbcnum (tp, col)
	    datatype = COL_DTYPE(cp)

	    if (datatype == TBL_TY_DOUBLE) {
		do row = row1, row2
		    Memd[COL_OFFSET(cp) + row - 1] = INDEFD

	    } else if (datatype == TBL_TY_INT) {
		do row = row1, row2
		    Memi[COL_OFFSET(cp) + row - 1] = INDEFI

	    } else if (datatype < 0) {
		do row = row1, row2 {
		    lenstr = -COL_DTYPE(cp)		# not including EOS
		    ip = (row - 1) * (lenstr + 1)	# including EOS
		    Memc[COL_OFFSET(cp) + ip] = EOS
		}
	    } else {
		call error (ER_TBCOLBADTYP, "tbznll:  bad datatype")
	    }
	}
end

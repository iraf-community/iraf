include <tbset.h>
include "tbtables.h"

# tbfnll -- set elements to undefined in a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfnll (tp, firstrow, lastrow)

pointer tp		# i: pointer to table descriptor
long	firstrow	# i: first row to be set to INDEF
long	lastrow		# i: last row to be set to INDEF
#--
pointer cp		# pointer to column descriptor
long	row1, row2	# firstrow, lastrow truncated to 1, nrows
long	row		# loop indexes for row and column numbers
int	col
long	nelem		# number of elements for a column
int	status		# zero is OK
long	c_1
pointer tbcnum()
long	tbcigl()
errchk	tbferr

begin
	c_1 = 1
	status = 0

	row1 = max (1, firstrow)
	row2 = min (TB_NROWS(tp), lastrow)

	do row = row1, row2 {

	    do col = 1, TB_NCOLS(tp) {

		cp = tbcnum (tp, col)

		nelem = tbcigl (cp, TBL_COL_LENDATA)

		call fspclu (TB_FILE(tp), col, row, c_1, nelem, status)
		if (status != 0)
		    call tbferr (status)
	    }
	}
end

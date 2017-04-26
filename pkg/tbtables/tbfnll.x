include <tbset.h>
include "tbtables.h"

# tbfnll -- set elements to undefined in a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfnll (tp, firstrow, lastrow)

pointer tp		# i: pointer to table descriptor
int	firstrow	# i: first row to be set to INDEF
int	lastrow		# i: last row to be set to INDEF
#--
pointer cp		# pointer to column descriptor
int	row1, row2	# firstrow, lastrow truncated to 1, nrows
int	row, col	# loop indexes for row and column numbers
int	nelem		# number of elements for a column
int	status		# zero is OK
pointer tbcnum()
int	tbcigi()
errchk	tbferr

begin
	status = 0

	row1 = max (1, firstrow)
	row2 = min (TB_NROWS(tp), lastrow)

	do row = row1, row2 {

	    do col = 1, TB_NCOLS(tp) {

		cp = tbcnum (tp, col)

		nelem = tbcigi (cp, TBL_COL_LENDATA)

		call fspclu (TB_FILE(tp), col, row, 1, nelem, status)
		if (status != 0)
		    call tbferr (status)
	    }
	}
end

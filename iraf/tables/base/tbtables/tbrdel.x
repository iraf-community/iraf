include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrdel -- delete rows
# This routine deletes a range of rows.
# NOTE:  The number of rows TB_NROWS(tp) will be modified by this routine
# if the range of rows to be deleted is within the table.  The physical
# disk space occupied by these rows will not be deallocated; the file
# will still be just as large as before.
#
# Phil Hodge, 23-Mar-1988  Subroutine created.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  6-Mar-1998  Modify to allow a row selector, if the rows
#			to be deleted are at the end of the table.

procedure tbrdel (tp, firstrow, lastrow)

pointer tp		# i: pointer to table descriptor
int	firstrow	# i: first row to be deleted
int	lastrow		# i: last row to be deleted
#--
int	row1		# actual first row to be deleted
int	nrows		# number of rows written to table
int	ndel		# number of rows to be deleted
int	rst_rownum()
errchk	tbxnll, tbynll, tbznll, tbfchp, tbrsft

begin
	if (firstrow < 1 || lastrow < 1)
	    call error (1, "tbrdel:  Row number less than one is invalid.")

	if (firstrow > lastrow)
	    return

	nrows = TB_NROWS(tp)

	if (TB_ROW_SELECT(tp) == YES) {

	    if (lastrow < TB_NSEL_ROWS(tp))
		call error (1,
	"Can't delete rows in the middle of a table that uses a row selector.")

	    if (firstrow > TB_NSEL_ROWS(tp))
		return				# nothing to do

	    row1 = rst_rownum (TB_ROWSET(tp), firstrow)
	    # and row2 = nrows

	    if (nrows - row1 > TB_NSEL_ROWS(tp) - firstrow)
		call error (1,
	"tbrdel:  Range of rows to delete includes non-selected rows.")

	    ndel = nrows - row1 + 1

	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxnll (tp, max (1, nrows-ndel+1), nrows)
	    else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
		call tbynll (tp, max (1, nrows-ndel+1), nrows)
	    else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
		call tbznll (tp, max (1, nrows-ndel+1), nrows)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfchp (tp, ndel)
	    else
		call error (ER_TBCORRUPTED, "tbrdel:  invalid table type")

	    TB_NROWS(tp) = max (0, nrows - ndel)

	} else {

	    ndel = min (nrows, lastrow) - firstrow + 1
	    # Shift the rows.
	    call tbrsft (tp, firstrow, -ndel)
	}

	TB_MODIFIED(tp) = true
end

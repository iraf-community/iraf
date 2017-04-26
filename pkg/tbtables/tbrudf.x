include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrudf -- set to undefined
# "Delete" entries in a table by setting each entry to the INDEF value
# appropriate for its datatype.
#
# Phil Hodge,  4-Nov-1993  Include check on valid row number.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 15-Jun-1995  Modify for FITS tables.
# Phil Hodge,  3-Jun-1996  Errchk tbfudf; call tbtwer if beyond EOF.
# Phil Hodge,  3-Mar-1998  Modify to allow for row selector.
# Phil Hodge, 26-Jun-1998  Change test (rownum < 1) to (selrow < 1).

procedure tbrudf (tp, colptr, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer colptr[ARB]		# i: array of pointers to column descriptors
int	numcols			# i: number of columns
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	rst_rownum()
errchk	rst_rownum, tbswer, tbfudf, tbxudf, tbyudf, tbzudf

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY,
			"tbrudf:  can't delete entries in readonly table")

	if (selrow < 1)
	    call error (1, "tbrudf:  row number must be > 0")

	# If the row is beyond EOF, write extra rows and update TB_NROWS.
	# Then we're done, because the new rows are already INDEF.
	if (TB_ROW_SELECT(tp) == YES) {

	    if (selrow > TB_NSEL_ROWS(tp)) {
		call tbswer (tp, selrow, rownum)
		return
	    }
	    rownum = rst_rownum (TB_ROWSET(tp), selrow)

	} else {

	    rownum = selrow
	    if (rownum > TB_NROWS(tp)) {
		call tbswer (tp, selrow, rownum)
		return
	    }
	}

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    call tbxudf (tp, colptr, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    call tbyudf (tp, colptr, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    call tbzudf (tp, colptr, numcols, rownum)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    call tbfudf (tp, colptr, numcols, rownum)
	else
	    call error (ER_TBCORRUPTED, "tbrudf:  table type is messed up")

	TB_MODIFIED(tp) = true
end

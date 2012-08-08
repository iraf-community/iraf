include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbeoff -- get offset to an element
# This function returns the offset to an element (a specific row and
# column) in a table.
#
# Phil Hodge, 14-Sep-1987  Function created.
# Phil Hodge, 28-Jul-1994  Add elnum to calling sequence.

long procedure tbeoff (tp, cptr, rownum, elnum)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	rownum			# i: row number
int	elnum			# i: element number
#--
long	offset			# the offset in char
int	sz_element		# size of one element (if entry is an array)
int	tbeszt()

begin
	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    offset = TB_BOD(tp) + (rownum-1) * TB_ROWLEN(tp) +
			COL_OFFSET(cptr)

	else if (TB_TYPE(tp) == TBL_TYPE_S_COL)
	    offset = TB_BOD(tp) + COL_OFFSET(cptr) * TB_ALLROWS(tp) +
			(rownum-1) * COL_LEN(cptr)

	else if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    return (0)			# offset is meaningless

	else
	    call error (ER_TBCORRUPTED,
		"tbeoff:  bad table type; table or memory corrupted?")

	if (elnum > 1) {
	    # Not the first element.  First get the size of one element.
	    switch (COL_DTYPE(cptr)) {
	    case TBL_TY_REAL:
		sz_element = SZ_REAL
	    case TBL_TY_DOUBLE:
		sz_element = SZ_DOUBLE
	    case TBL_TY_INT:
		sz_element = SZ_INT32
	    case TBL_TY_SHORT:
		sz_element = SZ_SHORT
	    case TBL_TY_BOOL:
		sz_element = SZ_BOOL
	    default:
		sz_element = tbeszt (cptr)	# character type
	    }

	    offset = offset + (elnum-1) * sz_element
	}

	return (offset)
end

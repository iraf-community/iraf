include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtclo -- close a table
# The files themselves are closed, and memory that was allocated for
# descriptors is released.
#
# Phil Hodge,  8-Oct-1987  TB_COLPTR is of type TY_POINTER.
# Phil Hodge, 16-Nov-1990  Eliminate calls to tbxclo, tbyclo.
# Phil Hodge, 13-Jan-1992  Add option for text table type.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 23-Dec-1994  Deallocate space for table and file or HDU names.
# Phil Hodge,  3-Apr-1995  Check TB_MODIFIED before calling tbtwsi.
# Phil Hodge,  2-Mar-1998  Deallocate memory for row & column selectors.
# Phil Hodge, 22-Mar-1999  Deallocate memory for host OS file name.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE;
#			move deallocation of comment buffer to tbzclo.

procedure tbtclo (tp)

pointer tp			# i: pointer to descriptor of table to be closed
#--
int	colnum			# Column number
errchk	tbfclo, tbvclo, tbzclo

begin
	if (tp == NULL)
	    return

	# Text file; write values back to file if not read-only, and
	# deallocate buffers for column data.
	if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    call tbzclo (tp)

	# If the table has been changed, rewrite the current size information.
	if (TB_MODIFIED(tp) && TB_FILE(tp) != NULL)
	    call tbtwsi (tp)

	# Check for FITS file.
	if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    call tbfclo (tp)

	# Close the file itself.
	if (TB_FILE(tp) != NULL)
	    call close (TB_FILE(tp))

	# Check for CDF file.
# ***
	if (TB_TYPE(tp) == TBL_TYPE_CDF)
	    ;	# call qp_close (TB_CD(tp))

	# Free memory used for row and column selectors.
	if (TB_ROW_SELECT(tp) == YES) {
	    if (TB_ROWSET(tp) != NULL)
		call rst_free (TB_ROWSET(tp))
	}

	if (TB_COLUMN_SELECT(tp) == YES) {
	    if (TB_SELCOL_PTR(tp) != NULL) {
		call tcs_close (Memi[TB_SELCOL_PTR(tp)], TB_NSEL_COLS(tp))
		call mfree (TB_SELCOL_PTR(tp), TY_POINTER)
	    }
	}

	# Free memory used for column descriptors.
	do colnum = 1, TB_NCOLS(tp) {
	    if (TB_COLINFO(tp,colnum) != NULL)
		call mfree (TB_COLINFO(tp,colnum), TY_STRUCT)
	}
	# Free memory for the array of pointers to column descriptors.
	if (TB_COLPTR(tp) != NULL)
	    call mfree (TB_COLPTR(tp), TY_POINTER)

	# Free memory for buffer for the indef record (null value for each col)
	if (TB_INDEF(tp) != NULL)
	    call mfree (TB_INDEF(tp), TY_CHAR)

	# Deallocate space for the CDF file or FITS HDU name.
	if (TB_EXTNAME_PTR(tp) != NULL)
	    call mfree (TB_EXTNAME_PTR(tp), TY_CHAR)

	# Deallocate space for the table name.
	if (TB_NAME_PTR(tp) != NULL)
	    call mfree (TB_NAME_PTR(tp), TY_CHAR)
	if (TB_OS_FILENAME_PTR(tp) != NULL)
	    call mfree (TB_OS_FILENAME_PTR(tp), TY_CHAR)

	# Free memory for the table descriptor.
	call mfree (tp, TY_STRUCT)
end

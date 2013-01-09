include <tbset.h>
include "tbtables.h"

# tbsopn -- call the row & column selector routines
# This routine determines the subset of rows and columns that were selected
# by expressions appended to the table name.
#
# Memory is allocated for TB_SELCOL_PTR(tp), which
# should be deallocated when closing the table.  Also, rst_free and
# tcs_close should be called to deallocate the row set and descriptors
# that were allocated by tcs_open.
#
# Phil Hodge,  3-Mar-1998  Subroutine created.

procedure tbsopn (tp, rowselect, colselect)

pointer tp		# i: pointer to table descriptor
char	rowselect[ARB]	# i: row selector string
char	colselect[ARB]	# i: column selector string
#--
pointer trsrows()
int	rst_nelem()
errchk	trsrows, tcs_open

begin
	if (rowselect[1] != EOS) {

	    # This creates a row set and checks each row of the table
	    # for a match with the row selector string.
	    TB_ROWSET(tp) = trsrows (tp, rowselect)

	    # This is the number of selected rows.
	    TB_NSEL_ROWS(tp) = rst_nelem (TB_ROWSET(tp))

	    TB_ROW_SELECT(tp) = YES	# row selection is in effect

	} else {

	    TB_ROW_SELECT(tp) = NO	# row selection is not in effect
	}

	if (colselect[1] != EOS) {

	    # Allocate enough space to select all columns.
	    TB_MAX_SELCOLS(tp) = TB_NCOLS(tp)
	    call malloc (TB_SELCOL_PTR(tp), TB_MAX_SELCOLS(tp), TY_POINTER)

	    call tcs_open (tp, colselect,
			TB_SELCOL(tp,1), TB_NSEL_COLS(tp), TB_MAX_SELCOLS(tp))

	    TB_COLUMN_SELECT(tp) = YES	# column selection is in effect

	} else {

	    TB_COLUMN_SELECT(tp) = NO	# column selection is not in effect

	}
end

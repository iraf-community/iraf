include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbswer -- write empty rows
# The purpose of this routine is to write empty (INDEF) rows beyond the
# current end of file if the specified row is larger than the number of
# rows already written to the table.  If the specified row is within the
# range of existing rows, the table itself will not be modified.
#
# For a row-ordered table if rownum > EOF additional records will be written
# to fill out the file to include the specified row.  For a column-ordered
# table there are two considerations:  if rownum > TB_NROWS and <= TB_ALLROWS
# then only TB_NROWS will be updated, but if rownum > TB_ALLROWS then tbtchs
# is called to rewrite the table and increase the allocated number of rows
# by a default amount.
# When putting a column of values to a table, the appropriate actual argument
# for rownum is the number of the last row to be written.
# This routine may only be called when writing to a table.
#
# Phil Hodge,  4-Mar-1998  Subroutine created based on tbtwer.

procedure tbswer (tp, selrow, rownum)

pointer tp		# i: pointer to table descriptor
int	selrow		# i: row number (or selected row number)
int	rownum		# o: actual row number
#--
int	nrows			# number of rows on entry to this routine
errchk	tbsrow, tbfwer, tbxwer, tbywer, tbzwer

begin
	# Convert selrow to rownum.  If rownum is past EOF and there is a
	# row selector in effect, add rows to the list of selected rows.
	call tbsrow (tp, selrow, rownum)

	nrows  = TB_NROWS(tp)
	if (rownum <= nrows)
	    return				# nothing further to do

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {

	    # Write INDEF record(s) at the end of the table file.
	    call tbxwer (tp, rownum)

	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {

	    # Increase the length of columns in the table.
	    call tbywer (tp, rownum)

	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

	    # Increase the size of the buffers for storing column values.
	    call tbzwer (tp, rownum)

	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {

	    # Write undefined entries for the new rows.
	    call tbfwer (tp, nrows, rownum)

	} else {
	    call error (ER_TBCORRUPTED,
		"tbswer:  bad table type; table or memory corrupted?")
	}

	TB_NROWS(tp) = rownum			# update TB_NROWS

	TB_MODIFIED(tp) = true
end

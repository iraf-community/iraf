include "tbtables.h"

# tbtwer -- write empty rows
# The purpose of this routine is to write empty (INDEF) rows beyond the
# current end of file if the specified row is larger than the number of
# rows already written to the table.  If the specified row is within the
# range of existing rows, the table itself will not be modified.
#
# If there is a row selector and selrow is larger than the current upper
# limit to the selected rows, then one or more new rows will be written to
# the end of the table, these new rows will be included in the list of
# selected rows, and TB_NROWS will be updated.  The number of new, empty
# rows to be added after the current end of file is selrow minus the
# current number of selected rows.
#
# When putting a column of values to a table, the appropriate actual argument
# for rownum is the number of the last row to be written.
#
# This routine may only be called when writing to a table.
#
# Phil Hodge, 17-Sep-1987  Subroutine created.
# Phil Hodge,  8-Mar-1988  Change name in error mess from tbeoff to tbtwer.
# Phil Hodge, 14-Jan-1992  Add option for text table type;
#			call tbtchs directly instead of through tbytsz.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 21-Jun-1995  Modify for FITS tables; set TB_MODIFIED to true.
# Phil Hodge,  3-Mar-1998  Replace with a call to tbswer.

procedure tbtwer (tp, selrow)

pointer tp		# i: pointer to table descriptor
int	selrow		# i: row number (or selected row number)
#--
int	rownum		# actual row number (ignored)
errchk	tbswer

begin
	# Write empty rows to the end of the table,
	# and add new rows to the list of selected rows.
	call tbswer (tp, selrow, rownum)
end

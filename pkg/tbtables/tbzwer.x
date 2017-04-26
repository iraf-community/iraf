include "tbtables.h"

define	NUM_EXTRA	1000	# number of extra "rows" to add for text file

# tbzwer -- write empty rows
# The purpose of this routine is to allocate more space for rows for a
# text table.  If the specified row is within the range of
# existing rows, the table itself will not be modified.
#
# If rownum is greater than TB_NROWS but less than TB_ALLROWS, then only
# TB_NROWS will be updated.  If rownum is greater than TB_ALLROWS, then
# tbzsiz will be called to reallocate space for the table columns.
#
# Phil Hodge,  4-Mar-1998  Subroutine created, extracted from tbtwer.
# Phil Hodge,  7-Jun-1999  Add TB_MAXPAR(tp) to calling sequence of tbzsiz.

procedure tbzwer (tp, rownum)

pointer tp		# i: pointer to table descriptor
int	rownum		# i: (actual) row number in table
#--
int	old_allrows	# current allocated number of rows
errchk	tbzsiz

begin
	if (rownum > TB_ALLROWS(tp)) {
	    old_allrows = TB_ALLROWS(tp)
	    TB_ALLROWS(tp) = rownum + NUM_EXTRA
	    call tbzsiz (tp, TB_MAXPAR(tp), old_allrows)
	}

	if (rownum > TB_NROWS(tp))
	    TB_NROWS(tp) = rownum
end

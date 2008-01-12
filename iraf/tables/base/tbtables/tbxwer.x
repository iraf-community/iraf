include "tbtables.h"

# tbxwer -- write empty rows
# The purpose of this routine is to write empty (INDEF) rows beyond the
# current end of file for a row-ordered table, if the specified row is
# larger than the number of rows already written to the table.  If the
# specified row is within the range of existing rows, the table itself
# will not be modified.
#
# Note that TB_NROWS will not be updated.
#
# Phil Hodge,  4-Mar-1998  Subroutine created, extracted from tbtwer.

procedure tbxwer (tp, rownum)

pointer tp		# i: pointer to table descriptor
int	rownum		# i: (actual) row number in table
#--
long	locn		# location for writing (at EOF)
int	k		# loop index
int	nrows		# number of rows on entry to this routine
int	rowlen		# record length
errchk	seek, write

begin
	nrows  = TB_NROWS(tp)
	if (rownum <= nrows)
	    return				# nothing to do

	rowlen = TB_ROWLEN(tp)
	locn = TB_BOD(tp) + nrows * rowlen	# this is the end of file
	do k = nrows+1, rownum {
	    call seek (TB_FILE(tp), locn)
	    call write (TB_FILE(tp), Memc[TB_INDEF(tp)], rowlen)
	    locn = locn + rowlen
	}
end

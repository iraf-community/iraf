include "tbtables.h"

# tbfchp -- chop rows off the end of a FITS table
# This routine deletes rows at the end of a table.
#
# Phil Hodge,  6-Mar-1998  Subroutine created.

procedure tbfchp (tp, ndel)

pointer tp		# i: pointer to table descriptor
int	ndel		# i: number of rows to be deleted
#--
int	nrows		# number of rows in the table before deleting
int	status
errchk	tbferr

begin
	nrows = TB_NROWS(tp)

	status = 0
	call fsdrow (TB_FILE(tp), max (1, nrows-ndel+1), ndel, status)
	if (status > 0)
	    call tbferr (status)

	# Change the value of TB_NROWS.
	TB_NROWS(tp) = max (0, nrows - ndel)
end

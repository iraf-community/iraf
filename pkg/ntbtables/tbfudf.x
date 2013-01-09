include <tbset.h>
include "tbtables.h"

# tbfudf -- set elements to undefined in a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfudf (tp, cp, numcols, rownum)

pointer tp		# i: pointer to table descriptor
pointer cp[ARB]		# i: array of pointers to column descriptors
int	numcols		# i: number of columns
int	rownum		# i: row number
#--
int	nelem		# number of elements for a column
int	i		# loop index
int	status		# zero is OK
int	tbcigi()
errchk	tbferr

begin
	status = 0

	do i = 1, numcols {

	    nelem = tbcigi (cp[i], TBL_COL_LENDATA)

	    call fspclu (TB_FILE(tp), COL_NUMBER(cp[i]), rownum,
			1, nelem, status)
	    if (status != 0)
		call tbferr (status)
	}
end

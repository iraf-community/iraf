include <tbset.h>
include "tbtables.h"

# tbfudf -- set elements to undefined in a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfudf (tp, cp, numcols, rownum)

pointer tp		# i: pointer to table descriptor
pointer cp[ARB]		# i: array of pointers to column descriptors
int	numcols		# i: number of columns
long	rownum		# i: row number
#--
long	nelem		# number of elements for a column
int	i		# loop index
int	status		# zero is OK
long	c_1
long	tbcigl()
errchk	tbferr

begin
	c_1 = 1
	status = 0

	do i = 1, numcols {

	    nelem = tbcigl (cp[i], TBL_COL_LENDATA)

	    call fspclu (TB_FILE(tp), COL_NUMBER(cp[i]), rownum,
			 c_1, nelem, status)
	    if (status != 0)
		call tbferr (status)
	}
end

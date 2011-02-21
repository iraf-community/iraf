include <tbset.h>
include "tbtables.h"

# tbfwsi -- write size info
# If the current HDU is a table, this routine writes the number of rows and
# the number of columns as FITS keywords NAXIS2 and TFIELDS respectively.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  2-Feb-1996  Check that current HDU is a table.
# Phil Hodge,  7-Jun-1999  Use TB_SUBTYPE instead of TB_HDUTYPE.

procedure tbfwsi (tp)

pointer tp		# i: pointer to table descriptor
#--
int	status		# zero is OK
errchk	tbferr

begin
	status = 0

	if (TB_SUBTYPE(tp) == TBL_SUBTYPE_BINTABLE || 
	    TB_SUBTYPE(tp) == TBL_SUBTYPE_ASCII) {

	    call fsmkyj (TB_FILE(tp), "NAXIS2", TB_NROWS(tp), "", status)
	    if (status != 0)
		call tbferr (status)

	    call fsmkyj (TB_FILE(tp), "TFIELDS", TB_NCOLS(tp), "", status)
	    if (status != 0)
		call tbferr (status)
	}
end

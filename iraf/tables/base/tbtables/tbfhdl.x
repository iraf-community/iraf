include "tbtables.h"

# tbfhdl -- delete a header keyword from a FITS table
# This routine deletes one header keyword (by number) from the current HDU
# in a FITS file.
#
# Phil Hodge,  3-Oct-1995  Subroutine created

procedure tbfhdl (tp, parnum)

pointer tp		# io: pointer to table descriptor
int	parnum		# i: number of the parameter to be deleted
#--
int	status		# zero is OK
errchk	tbferr

begin
	status = 0

	# Delete the keyword.
	call ftdrec (TB_FILE(tp), parnum, status)

	if (status != 0)
	    call tbferr (status)

	TB_NPAR(tp) = TB_NPAR(tp) - 1
end

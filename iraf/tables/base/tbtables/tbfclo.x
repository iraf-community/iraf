include <tbset.h>
include "tbtables.h"

# tbfclo -- close a table in a FITS file
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  1-Jun-1999  Set both TB_FILE and TB_FILE2 to 0.

procedure tbfclo (tp)

pointer tp		# i: pointer to table descriptor
#--
int	status
errchk	tbferr

begin
	if (TB_FILE(tp) == 0)
	    return

	# Close the file, and free the unit number.
	status = 0
	call fsclos (TB_FILE(tp), status)
	call fsfiou (TB_FILE(tp), status)
	if (status != 0)
	    call tbferr (status)
	TB_FILE(tp) = 0
	TB_FILE2(tp) = 0
end

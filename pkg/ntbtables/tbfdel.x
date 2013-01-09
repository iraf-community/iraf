include "tbtables.h"

# tbfdel -- delete FITS table
# This routine deletes the current HDU in a FITS file, closes the FITS
# file, and frees the memory allocated by the table I/O routines.
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfdel (tp)

pointer tp		# io: pointer to table descriptor
#--
int	status		# zero is OK
int	hdutype		# type of next HDU (ignored)
errchk	tbferr

begin
	status = 0

	# Delete the current HDU.
	call fsdhdu (TB_FILE(tp), hdutype, status)

	if (status != 0)
	    call tbferr (status)

	# Close the FITS file.  This sets TB_FILE to NULL.
	call tbfclo (tp)

	# Free memory.
	call tbtclo (tp)
end

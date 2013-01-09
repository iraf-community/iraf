include "tblerr.h"

define	SZ_SH_STR	21	# local buffer size

# Convert a data type expressed as a character string to an integer.
# This version is for text tables, so the data type is coerced if
# necessary into one of the types supported for text tables.
#
# Phil Hodge, 24-Sep-1999  Subroutine created.

procedure tbztyp (chdtype, datatype)

char	chdtype[ARB]		# i: data type expressed as a string
int	datatype		# o: data type expressed as an int
#--
errchk	tbbtyp

begin
	call tbbtyp (chdtype, datatype)

	if (datatype == TY_REAL)
	    datatype = TY_DOUBLE
	else if (datatype == TY_SHORT)
	    datatype = TY_INT
	else if (datatype == TY_BOOL)
	    datatype = -8		# character*8
end

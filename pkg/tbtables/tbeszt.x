include <mach.h>		# needed for SZB_CHAR
include "tbtables.h"

# tbeszt -- length of char element
# This routine returns the amount of space (unit = char) taken up in a
# table file by a character string.  This may be different from the length
# of the string as would be returned by the strlen function, say, because
# the element in the table is packed, and the space is rounded up to a
# multiple of SZB_CHAR bytes.
#
# Phil Hodge, 28-Jul-1994  Function created.
# Phil Hodge, 31-Oct-1994  Check for data type = TY_CHAR.

int procedure tbeszt (cptr)

pointer cptr			# i: pointer to column descriptor
#--

begin
	if (COL_DTYPE(cptr) == TY_CHAR)		# old notation
	    return (COL_LEN(cptr))
	else
	    return ((-COL_DTYPE(cptr) + SZB_CHAR-1) / SZB_CHAR)
end

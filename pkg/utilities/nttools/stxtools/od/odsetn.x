include <imhdr.h>
include "od.h"

#---------------------------------------------------------------------------
.help od_set_len Jun93 source
.ih
NAME
od_set_len -- Set the length of data.
.ih
DESCRIPTION
This sets how much data is read/written from the OD file.  For images,
the dimensionality is changed.	For tables, it just changes how much
is read/written; nothing is physically changed about the table.
.endhelp
#---------------------------------------------------------------------------
procedure od_set_len (od, len)

pointer	od			# I: OD descriptor.
int	len			# I: New length.

begin
	OD_LEN(od) = len
	if (OD_TYPE(od) == OD_IMAGE) {
	    IM_LEN(OD_FD(od),1) = len
	}
end
#---------------------------------------------------------------------------
# End of od_set_len
#---------------------------------------------------------------------------

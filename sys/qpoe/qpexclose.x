# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpex.h"

# QPEX_CLOSE -- Close the QPEX descriptor.

procedure qpex_close (ex)

pointer	ex			#I QPEX descriptor

pointer	lt

begin
	# Free any LUT buffers.
	for (lt=EX_LTHEAD(ex);  lt != NULL;  lt=LT_NEXT(lt))
	    call mfree (LT_LUTP(lt), TY_SHORT)

	# Free the data and program buffers.
	call mfree (EX_PB(ex), TY_STRUCT)
	call mfree (EX_DB(ex), TY_CHAR)

	# Free the main descriptor.
	call mfree (ex, TY_STRUCT)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"

# ZCLM75 -- Close and deallocate the IIS.

procedure zclm75 (ifcb, status)

int	ifcb			# pointer to channel descriptor passed as int
int	status
pointer	fcb

begin
	fcb = ifcb
	call zclsbf (FCB_CHAN(fcb), status)

	call mfree (fcb, TY_STRUCT)
end

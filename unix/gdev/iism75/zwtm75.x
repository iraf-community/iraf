# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"

# ZWTM75 -- Wait for i/o completion and return the number of bytes read or
# written or ERR.  Repetitive calls return the same value.

procedure zwtm75 (ifcb, status)

int	ifcb			# pointer to channel descriptor passed as int
int	status			# nbytes transferred or ERR

pointer	fcb

begin
	fcb = ifcb

	switch (FCB_STATUS(fcb)) {
	case ERR:
	    status = ERR
	case IIS_INACTIVE:
	    status = FCB_NBYTES(fcb)

	default:
	    call zawtbf (FCB_CHAN(fcb), status)
	    FCB_STATUS(fcb) = IIS_INACTIVE
	}
end

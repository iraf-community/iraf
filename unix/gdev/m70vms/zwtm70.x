# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include	"m70.h"

# ZWTM70 -- Wait for i/o completion and return the number of bytes read or
# written or ERR.  Repetitive calls return the same value.

procedure zwtm70 (chan, status)

int	chan			# FCB pointer for device
int	status			# nbytes transferred or ERR

pointer	fcb
short	bfnum, bufcnt, ier

begin
	fcb = chan

	if (FCB_KCHAN(fcb) == NULL) {
	    switch (FCB_STATUS(fcb)) {
	    case ERR:
		status = ERR
	    case IIS_INACTIVE:
		status = FCB_NBYTES(fcb)

	    default:
		bfcnt = -1		# m70wt is a nop if we don't do this
		bfnum = FCB_EFN(fcb)

		call m70wt (Mems[fcb], bfnum, bfcnt, FCB_IOSB(fcb,1), ier)

		if (ier != 0)
		    status = ERR
		else
		    status = FCB_NBYTES(fcb)

		FCB_STATUS(fcb) = IIS_INACTIVE
	    }

	} else
	    call zawtbf (FCB_KCHAN(fcb), status)
end

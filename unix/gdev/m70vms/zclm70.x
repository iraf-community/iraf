# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include	"m70.h"

# ZCLM70 -- Close and deallocate the IIS.

procedure zclm70 (chan, status)

int	chan			# FCB pointer for device
int	status
pointer	fcb

begin
	fcb = chan
	if (FCB_KCHAN(fcb) == NULL) {
	    call zwtm70 (chan, status)
	    call m70rel (Mems[fcb])
	} else
	    call zclsbf (FCB_KCHAN(fcb), status)

	call mfree (fcb, TY_SHORT)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <knet.h>
include <fio.h>
include	"iis.h"

# ZSTTIM -- Return status on binary file display device.

procedure zsttim (chan, what, lvalue)

int	chan[ARB], what
long	lvalue

include	"iis.com"

begin
	call zsttgd (iischan, what, lvalue)
		
	if (what == FSTT_MAXBUFSIZE) {
	    # Return the maximum transfer size in bytes.
	    if (lvalue == 0)
		lvalue = FSTT_MAXBUFSIZE
	    if (!packit)
		lvalue = min (IIS_MAXBUFSIZE, lvalue) * 2
	}
end

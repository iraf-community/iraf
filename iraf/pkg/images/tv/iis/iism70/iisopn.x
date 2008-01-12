# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "iis.h"

# IISOPN -- Open IIS display.

procedure iisopn (devinfo, mode, chan)

char	devinfo[ARB]		# device info for zopen
int	mode			# access mode
int	chan[ARB]		# receives IIS descriptor

bool	first_time
data	first_time /true/
include	"iis.com"

begin
	if (first_time) {
	    iisnopen = 0
	    first_time = false
	}

	# We permit multiple opens but only open the physical device once.
	if (iisnopen == 0)
	    call zopngd (devinfo, mode, iischan)

	if (iischan == ERR)
	    chan[1] = ERR
	else {
	    iisnopen = iisnopen + 1
	    chan[1]  = iischan
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "zdisplay.h"
include "iis.h"

# IISWT -- Wait for IIS display.

procedure iiswt (chan, nbytes)

int	chan[ARB], nbytes
include	"iis.com"

begin
	call zawtgd (iischan, nbytes)
	if (packit)
	    nbytes = nbytes * (SZ_SHORT * SZB_CHAR)
end

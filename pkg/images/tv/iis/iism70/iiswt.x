# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include "iis.h"

# IISWT -- Wait for IIS display.

procedure iiswt (chan, nbytes)

int	chan[ARB]
int	nbytes
include	"iis.com"

begin
	call zawtgd (iischan, nbytes)
	nbytes = nbytes * SZB_CHAR
end

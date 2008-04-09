# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "iis.h"

# IISGOP -- Open IIS graphics display.

procedure iisgop (frame, mode, chan)

int	frame, mode, chan[ARB]

begin
	# ??? what is this: arg1 ???
	call iisopn (frame + LEN_IISFRAMES, mode, chan)
end

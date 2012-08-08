# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "iis.h"

# IISGOP -- Open IIS graphics display.

procedure iisgop (frame, mode, chan)

int	frame, mode, chan[ARB]

begin
	call iisopn (frame + LEN_IISFRAMES, mode, chan)
end

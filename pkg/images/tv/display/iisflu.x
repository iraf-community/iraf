# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "zdisplay.h"
include	"iis.h"

# IISFLU -- IIS frame look up table.

int procedure iisflu (chan)

int	chan[ARB]
int	frame
int	iisframe[LEN_IISFRAMES]
data	iisframe/IISFRAMES/

begin
	frame = chan[1] - IIS_CHAN * DEVCODE
	if (frame < 1)
	    return (iisframe[1])
	else if (frame > LEN_IISFRAMES)
	    return (GRCHAN)
	else
	    return (iisframe[frame])
end

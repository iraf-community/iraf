# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "zdisplay.h"
include "iis.h"

# IISMTC -- Match channel lut to frame2.

procedure iismtc (chan1, chan2)

int	chan1[ARB], chan2[ARB]
short	lut[LEN_LUT]

int	iisflu()

begin
	if (iisflu (chan2) == GRCHAN)
	    return
	call iisrlt (chan1, lut)
	call iiswlt (chan2, lut)
end

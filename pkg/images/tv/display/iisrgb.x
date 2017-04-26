# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISRGB -- Enable RGB display.

procedure iisrgb (red_chan, green_chan, blue_chan)

int	red_chan[ARB], green_chan[ARB], blue_chan[ARB]

int	i, frm, status
short	split[LEN_SPLIT]
int	iisflu()

begin
	frm = iisflu (blue_chan)
	do i = 1, 4
	    split[i] = frm

	frm = iisflu (green_chan)
	do i = 5, 8
	    split[i] = frm

	frm = iisflu (red_chan)
	do i = 9, 12
	    split[i] = frm

	call iishdr (IWRITE+VRETRACE, LEN_SPLIT, COMMAND+LUT, ADVXONTC, 0, 0, 0)
	call iisio (split, LEN_SPLIT * SZB_CHAR, status)
end

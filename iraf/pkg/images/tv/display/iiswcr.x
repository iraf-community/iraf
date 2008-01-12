# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISWCR -- Write cursor to display.  Note that the position is 1 indexed.

procedure iiswcr (status, xcur, ycur)

int	status, xcur, ycur
short	cursor[LEN_CURSOR]
include	"iis.com"

begin
	call iishdr (IWRITE+VRETRACE, 2, COMMAND+CURSOR, 1+ADVXONTC, 0,0,0)
	cursor[2] = mod (xcur / MCXSCALE - 32, iis_xdim)
	cursor[3] = mod (ycur / MCYSCALE - 32, iis_ydim)
	call iisio (cursor[2], 2 * SZB_CHAR, status)
end

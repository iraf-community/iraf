# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISWCR -- Write cursor to display.  Note that the position is 1 indexed.

procedure iiswcr (status, xcur, ycur)

int	status
int	xcur
int	ycur

short	cursor[LEN_CURSOR]
int	modi()
include	"iis.com"

begin
	call iishdr (IWRITE+VRETRACE, 2, COMMAND+CURSOR, 1+ADVXONTC, 0,0,0)
	cursor[2] = modi (xcur / MCXSCALE - 32, iis_xdim)
	cursor[3] = modi (ycur / MCYSCALE - 32, iis_ydim)
	call iisio (cursor[2], 2 * SZB_CHAR, status)
end

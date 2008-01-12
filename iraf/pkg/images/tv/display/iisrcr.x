# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

define	DELAY	30		# milliseconds between cursor reads


# IISRCR -- Read cursor from display.  Note that the position is 1 indexed.

procedure iisrcr (status, xcur, ycur)

int	status, xcur, ycur
short	cursor[LEN_CURSOR]
include	"iis.com"

begin
	call iishdr(IREAD+VRETRACE, LEN_CURSOR, COMMAND+CURSOR, ADVXONTC, 0,0,0)

	call zwmsec (DELAY)

	call iisio (cursor, LEN_CURSOR * SZB_CHAR, status)
	if (status <= 0) {
	    status = EOF
	    return
	}

	status = cursor[1]
	xcur = MCXSCALE * mod (cursor[2] + 31, iis_xdim)
	ycur = MCYSCALE * mod (cursor[3] + 31, iis_ydim)
end

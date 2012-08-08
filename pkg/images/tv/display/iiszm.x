# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISZM -- Zoom IIS window.

procedure iiszm (zfactor, x, y)

int	zfactor, x, y
short	zoom[LEN_ZOOM]
int	status

begin
	call iishdr (IWRITE+VRETRACE, LEN_ZOOM, ZOOM, ADVXONTC, 0, 0, 0)
	zoom[1] = zfactor - 1
	zoom[2] = x / MCXSCALE
	zoom[3] = y / MCYSCALE
	call iisio (zoom, LEN_ZOOM * SZB_CHAR, status)
end


# IISRM -- Roam IIS display.

procedure iisrm (zfactor)

int	zfactor
int	status, xcur, ycur
int	and()

begin
	status = 0
	while (status != EOF && and (status, PUSH) == 0) {
	    call iisrcr (status, xcur, ycur)
	    call iiszm (zfactor, xcur, ycur)
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISRD -- Read data from IIS.

procedure iisrd (chan, buf, nbytes, offset)

int	chan[ARB]
short	buf[ARB]
int	nbytes
long	offset

long	off1, off2
int	nchars, thing_count, tid, y1, y2, x
int	or(), iisflu()
include	"iis.com"

begin
	# Convert to chars and clip at the top of the display.
	off1 = (offset - 1) / SZB_CHAR + 1
	off2 = min (iis_xdim * iis_ydim, (offset + nbytes - 1) / SZB_CHAR) + 1
	nchars = off2 - off1

	x  = 0
	y1 = (off1-1           ) / iis_xdim
	y2 = (off2-1 - iis_xdim) / iis_xdim
	y2 = max (y1, y2)

	if (packit)
	    tid = IREAD+PACKED
	else
	    tid = IREAD
	thing_count = nchars

	call iishdr (tid, thing_count, REFRESH, or(x,ADVXONTC),
	    or(iis_ydim-y2-1, ADVYONXOV), iisflu(chan), ALLBITPL)

	call iispio (buf, iis_xdim, y2 - y1 + 1)
end

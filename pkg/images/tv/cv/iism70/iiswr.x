# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"

# IISWR -- Write pixel data to IIS.  Writes are packed with full lines only.
# The data is line-flipped, causing the first line to be displayed at the bottom
# of the screen.

procedure iiswr (chan, buf, nbytes, offset)

int	chan[ARB]
short	buf[ARB]
int	nbytes
long	offset

long	off1, off2
int	nchars, thing_count, tid, y1, y2, x
int	or()
include "iis.com"

begin
	# Convert to chars and clip at the top of the display.
	off1 = (offset - 1) / SZB_CHAR + 1
	off2 = min (IIS_XDIM * IIS_YDIM, (offset + nbytes - 1) / SZB_CHAR) + 1
	nchars = off2 - off1

	y1 = (off1-1           ) / IIS_XDIM
	y2 = (off2-1 - IIS_XDIM) / IIS_XDIM
	y2 = max (y1,y2)

	# Pack only if full lines
	x = (off1 - 1) - y1 * IIS_XDIM
	if ( x == 0 )
	    tid = IWRITE+BYPASSIFM+PACKED+BLOCKXFER+BYTE
	else
	    tid = IWRITE+BYPASSIFM

	# If only a few chars, don't pack (BLOCKXFER needs nchar>=4)
	if ( nchars < 4 )
	    tid = IWRITE+BYPASSIFM

	thing_count = nchars

	call iishdr (tid, thing_count, REFRESH,
	     or (x, ADVXONTC), or (IIS_YDIM-1-y2, ADVYONXOV), iframe, iplane)
	if ( tid == IWRITE+BYPASSIFM)
	    call iisio (buf, nbytes)
	else
	    call iispio (buf, y2 - y1 + 1)
end

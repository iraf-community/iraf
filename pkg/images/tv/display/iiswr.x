# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISWR -- Write pixel data to IIS.  Writes are limited to entire display lines.
# The data is line-flipped, causing the first line to be displayed at the bottom
# of the screen.

procedure iiswr (chan, buf, nbytes, offset)

int	chan[ARB]		# io channel
short	buf[ARB]		# pixels
int	nbytes			# length of pixel array in bytes
long	offset			# pixel offset in image display

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

#call eprintf ("iiswr: %d bytes at %d, x=%d, y=[%d:%d]\n")
#call pargi(nbytes); call pargi(offset)
#call pargi(x); call pargi(y1); call pargi(y2)

	if (packit)
	    tid = IWRITE+BYPASSIFM+BLOCKXFER+BYTE+PACKED
	else
	    tid = IWRITE+BYPASSIFM
	thing_count = nchars

	call iishdr (tid, thing_count, REFRESH, or(x,ADVXONTC),
	    or(iis_ydim-y2-1, ADVYONXOV), iisflu(chan), ALLBITPL)

	call iispio (buf, iis_xdim, y2 - y1 + 1)
end

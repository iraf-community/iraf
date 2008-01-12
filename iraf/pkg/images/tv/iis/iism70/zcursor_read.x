# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <gki.h>
include "iis.h"
include	"../lib/ids.h"

# ZCURSOR_READ -- Read cursor from display.  This assumes that the cursor
# is centered at  (31,31)

procedure zcursor_read (cnum, xcur, ycur, key)

int	cnum			# cursor number
int	xcur, ycur		# cursor position...GKI coordinates
int	key			# key pressed

short	cursor[2]		# local storage
real	x,y
int	frame
real	zm
int	mod(), and()
define	exit_	10

include "iis.com"

begin
	# Computations must be done in floating point when zoomed
	# or values are off by a pixel.  Also, want fractional
	# pixel returned values in the zoomed case.

	call iishdr(IREAD, 2, COMMAND+CURSOR, 1+ADVXONTC, 0,0,0)
	call iisio (cursor, 2 * SZB_CHAR)

	# which frame is the cursor relative to?  We assume that cnum
	# mod IDS_CSET refers to the image plane (graphics fits in
	# here as an image plane for iism70), and cnum / IDS_CSET
	# sets which cursor.
	# If cursor is #0, then take lowest numbered frame that is
	# being displayed.
	# Return frame number as the "key".

	if (cnum == 0) {
	    frame = i_frame_on
	    if ((frame == ERR) || (frame < 1) ) {
		key = ERR
		return
	    }
	} else if (cnum != IDS_CRAW) {
	    frame = mod(cnum-1, IDS_CSET) + 1
	} else {
	    zm = 1.
	    frame = 0			# return unusual frame num. if raw read
	}

	# deal with cursor offset--hardware fault sometimes adds extra
	# bit, so chop it off with and().
	x = mod (and (int(cursor[1]), 777B)+ 31, 512)
	y = mod (and (int(cursor[2]), 777B)+ 31, 512)

	if (cnum == IDS_CRAW)
	    goto exit_

	# x,y now in device coordinates for screen but not world.
	# next, we determine number of pixels from screen center.

	zm = zoom[frame]
	x = x/zm - IIS_XCEN./zm
	y = y/zm - IIS_YCEN_INV./zm

	# Now add in scroll offsets, which are to screen center.
	x = x + xscroll[frame]

	# Note that the Y one is inverted
	y = y + (IIS_YDIM-1) - yscroll[frame]

	if (x < 0)
	    x = x + IIS_XDIM
	else if (x > IIS_XDIM)
	    x = x - IIS_XDIM

	if (y < 0)
	    y = y + IIS_YDIM
	else if (y > IIS_YDIM)
	    y = y - IIS_YDIM
exit_
	# invert y for user
	y = (IIS_YDIM -1) - y

	# The Y inversion really complicates things...
	y = y + 1.0 - (1.0/zm)

	# convert to GKI
	xcur = x * MCXSCALE
	ycur = y * MCYSCALE
	key = frame
end

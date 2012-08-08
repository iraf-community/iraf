# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <gki.h>
include "iis.h"
include "../lib/ids.h"

# IISSCROLL -- Read and Write scroll registers
# We scroll multiple frames to multiple centers; if there are not
# enough data pairs to match the number of frames, use the last
# pair repeatedly.

procedure iisscroll (rw, frame, n, data)

short	rw			# read or write
short	frame[ARB]		# frame data
short	n			# number of data values
short	data[ARB]		# the data

int	z
short	iispack()
int	i,total, pl, index

include "iis.com"

begin
	total = n/2
	if ( rw != IDS_WRITE) {
	    # Scroll registers are write only
	    do i = 1, total {
		pl = frame[i]
		if (pl == IDS_EOD)
		    break
		data[2*i-1] = xscroll[pl] * MCXSCALE
		data[2*i]   = yscroll[pl] * MCYSCALE
	    }

	    if (2*total < n)
		data[2*total+1] = IDS_EOD
	    return
	}

	# Set all the scroll offsets.
	index = 1
	for (i=1;  frame[i] != IDS_EOD;  i=i+1) {
	    pl = frame[i]
	    xscroll[pl] = data[2*index-1] / MCXSCALE
	    yscroll[pl] = data[2*index  ] / MCYSCALE
	    if (i < total)
		index = index + 1
	}

	# Now do the scrolling.
	for (i=1;  frame[i] != IDS_EOD;  i=i+1) {
	    pl = frame[i]
	    if (i == total) {
		z = iispack (frame[i])
		call do_scroll (z, xscroll[pl], yscroll[pl])
		break
	    } else
		call do_scroll (short(2**(pl-1)), xscroll[pl], yscroll[pl])
	}
end


procedure do_scroll (planes, x, y)

short	planes				# bit map for planes
short	x,y				# where to scroll

short	command
short	scr[2]
short	xs,ys

include	"iis.com"

begin
	xs = x
	ys = y
	command = IWRITE+VRETRACE
	scr[1] = xs
	scr[2] = ys

	# If x/y scroll at "center", scr[1/2] are now IIS_[XY]CEN
	# y = 0 is at top for device while y = 1 is bottom for user
	# so for y, center now moves to IIS_YCEN_INV !!

	scr[2] = IIS_YDIM - 1 - scr[2]

	# Scroll is given for center, but hardware wants corner coords.
	scr[1] = scr[1] - IIS_XCEN
	scr[2] = scr[2] - IIS_YCEN_INV

	if (scr[1] < 0)
	    scr[1] = scr[1] + IIS_XDIM
	if (scr[2] < 0)
	    scr[2] = scr[2] + IIS_YDIM

	call iishdr (command, 2, SCROLL, ADVXONTC, 0, int(planes), 0)
	call iisio (scr, 2 * SZB_CHAR)
end

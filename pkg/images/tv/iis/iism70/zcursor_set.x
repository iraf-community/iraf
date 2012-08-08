# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <gki.h>
include	"../lib/ids.h"
include "iis.h"

# ZCURSOR_SET -- Write cursor to display.  This code assumes the standard
# cursor which is centered on (31,31).

procedure zcursor_set (cnum, xcur, ycur)

int	cnum			# cursor number
int	xcur, ycur		# GKI x,y cursor position

short	cursor[2]		# local storage
real	x,y,zm
int	xedge
int	yedge, frame
int	mod()
define	output	10

include "iis.com"

begin
	# which frame does cursor refer to?  ( see zcursor_read() for
	# more information. )

	if (cnum == IDS_CRAW) {
	    x = real(xcur)/MCXSCALE
	    y = real(ycur)/MCYSCALE
	    zm = 1
	    xedge = 0
	    yedge = 0
	    goto output
	}

	if (cnum == 0) {
	    frame = i_frame_on
	    if ((frame == ERR) || (frame < 1))
		return			# WHAT SHOULD WE DO?
	} else
	    frame = mod( cnum-1, IDS_CSET) + 1
	zm = zoom[frame]

	# Find the left/upper edge of the display
	# xedge is real as we can't drop the fraction of IIS_XCEN/zm
	# (This was true when XCEN was 255; now is 256 so can use int
	# since 256 is a multiple of all possible values of zm.)

	xedge = xscroll[frame] - IIS_XCEN/zm
	if (xedge < 0)
	    xedge = xedge + IIS_XDIM
	yedge = ( (IIS_YDIM-1) - yscroll[frame]) - int(IIS_YCEN_INV/zm)
	if (yedge < 0)
	    yedge = yedge + IIS_YDIM

	# xcur, ycur are in gki.  Check if value too big...this will
	# happen if NDC = 1.0, for instance which should be acceptable
	# but will be "out of range".

	x = real(xcur)/MCXSCALE
	if ( x > (IIS_XDIM - 1.0/zm) )
	    x = IIS_XDIM - 1.0/zm
	y = real(ycur)/MCYSCALE
	if ( y > (IIS_YDIM - 1.0/zm) )
	    y = IIS_YDIM - 1.0/zm

	# Invert y value to get device orientation; account for
	# fractional pixels

output
	y = (IIS_YDIM - 1.0/zm) - y

	# Account for the mod 512 nature of the display

	if (x < xedge)
	    x = x + IIS_XDIM
	if (y < yedge)
	    y = y + IIS_YDIM

	# Are we still on screen ?

	if ((x >= (xedge + IIS_XDIM/zm)) || (y >= (yedge + IIS_YDIM/zm)) ) {
	    call eprintf("cursor set off screen -- ignored\n")
	    return
	}

	# Calculate cursor positioning coordinates.

	cursor[1] = int ((x-real(xedge)) * zm ) - 31
	if ( cursor[1] < 0 )
	    cursor[1] = cursor[1] + IIS_XDIM
	cursor[2] = int ((y-real(yedge)) * zm ) - 31
	if ( cursor[2] < 0 )
	    cursor[2] = cursor[2] + IIS_YDIM

	call iishdr (IWRITE+VRETRACE, 2, COMMAND+CURSOR, 1+ADVXONTC, 0,0,0)
	call iisio (cursor, 2 * SZB_CHAR)
end

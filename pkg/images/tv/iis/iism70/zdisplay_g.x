# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../lib/ids.h"
include "iis.h"

define	INSERT		100000B

# ZDISPLAY_G -- Display the referenced graphics bitplanes in the given color(s)

procedure zdisplay_g (sw, bitpl, color, quad )

short	sw			# on or off
short	bitpl[ARB]		# bitpl list
short	color[ARB]		# color list
short	quad[ARB]		# quadrants to activate

short	gram[LEN_GRAM]
bool	off
int	i, lbound, val
short	mask[7]
short	fill
# red a bit weak so have contrast with cursor
#colors of graph: blue  grn   red    yellow  rd-bl   gn-bl  white
data	mask    /37B, 1740B, 74000B, 77740B, 74037B, 1777B, 77777B/

begin
	if ( sw == IDS_OFF ) 
	    off = true
	else {
	    off = false
	}

	# ignore bitpl argument since only one set of them and "color"
	# fully specifies them.
	# ignore quad for now
	# much manipulation of color graphics ram table required!!
	# strictly speaking, when we turn a plane off, we ought to be
	# sure that any plane which is on, and "beneath", is turned on;
	# this is a lot of trouble, so for starters, we don't.
	# first find out what is on

	call iishdr(IREAD+VRETRACE, LEN_GRAM, GRAPHICS, ADVXONTC, 0, 0, 0)
	call iisio (gram, LEN_GRAM * SZB_CHAR)

	# Check for red graphics plane for cursor

	if ( gram[LEN_GRAM/2+1] != 176000B )
	    call amovks ( short(176000B), gram[LEN_GRAM/2+1], LEN_GRAM/2)

	for ( i = 1 ;  color[i] != IDS_EOD ; i = i + 1 ) {
	    # Bit plane 8 reserved for cursor
	    if ( color[i] > 7 )
		next
	    # map IDS colors to IIS bit planes -- one-based.
	    switch (color[i]) {
		case IDS_RED:
		    val = RD
		case IDS_GREEN:
		    val = GR
		case IDS_BLUE:
		    val = BLU
		default:
		    val = color[i]
	    }
	    lbound = 2 ** (val - 1)
	    if ( off ) 
		call aclrs ( gram[lbound+1], lbound)
	    else
		call amovks ( short(INSERT+mask[val]), gram[lbound+1], lbound)
	}
	gram[1] = 0

	# If a bit plane is off, reset it with next "lower" one, thus
	# uncovering any planes masked by the one turned off.

	if (off) {
	    fill = 0
	    do i = 2, LEN_GRAM/2 {
		if (gram[i] == 0 )
		    gram[i] = fill
		else
		    fill = gram[i]
	    }
	}

	# Write out the data

	call iishdr(IWRITE+VRETRACE, LEN_GRAM, GRAPHICS, ADVXONTC, 0, 0, 0)
	call iisio (gram, LEN_GRAM * SZB_CHAR)
end

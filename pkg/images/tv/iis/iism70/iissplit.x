# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	X_SPLIT	12

# IISSPLIT -- Read and Write split screen coordinates

procedure iissplit (rw, n, data)

short	rw			# read or write
short	n			# number of data values
short	data[ARB]		# the data

int	command,len,x
short	coord[2]

include "iis.com"

begin
	len = min (int(n), 2)
	if ( len < 1) {
	    data[1] = IDS_EOD
	    return
	}

	if (rw == IDS_WRITE) {
	    if (data[1] == IDS_EOD)
	        return
	    command = IWRITE+VRETRACE
	    coord[1] = data[1] / MCXSCALE


	    # Split screen will display the full screen from one lut ONLY
	    # if the split coordinate is zero.  Setting the split to 511
	    # means that all the screen BUT the last pixel is from one lut.
	    # Hence the y coordinate for full screen in one quad is 
	    # (device) 0 , (user) 511.  If the user requests split at (0,0),
	    # we honor this as a (device) (0,0).  This will remove the
	    # ability to split the screen with just the bottom line
	    # in the "other" lut, which shouldn't bother anyone.

	    if (len == 2)
		coord[2] = (IIS_YDIM - 1) - data[2]/MCYSCALE

	    if (coord[2] == IIS_YDIM - 1)
		coord[2] = 0

	} else
	    command = IREAD+VRETRACE

	# at most, read/write the x,y registers
	x = X_SPLIT + ADVXONTC

	call iishdr (command, len, LUT+COMMAND, x, 0, 0, 0)
	call iisio (coord, len * SZB_CHAR)

	if ( rw != IDS_WRITE ) {
	    data[1] = coord[1] * MCXSCALE
	    if ( len == 2 ) {
		if ( coord[2] == 0)
		    coord[2] = IIS_YDIM - 1
		data[2] = (IIS_YDIM - 1 - coord[2] ) * MCYSCALE
	    }
	}
end

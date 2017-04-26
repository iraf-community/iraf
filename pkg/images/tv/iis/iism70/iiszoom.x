# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

# IISZOOM -- Read and Write zoom magnification and coordinates.
# the zoom coordinates give the point that should appear in the
# center of the screen.  For the I2S model 70, this requires a
# scroll.  In order for the scroll to be "determinable", we always
# set the I2S "zoom center" to (IIS_XCEN,IIS_YCEN_INV).  The IIS_YCEN_INV
# results from specifying IIS_YCEN for y center and then having to "invert" y
# to put GKI(y) = 0 at bottom.
# This routine implements a command of the form "zoom these frames
# to the coordinates given, with each triple of data setting a
# zoom factor and a zoom center for the corresponding frame".
# If there are excess frames (rel. to "n"), use the last triple.

procedure iiszoom (rw, frames, n, data)

short	rw			# read or write
short	frames[ARB]		# which frames to zoom
short	n			# number of data values
short	data[ARB]		# the data

int	command,x
int	i, total,pl,index
short	zm,temp[4]
short	scroll[2*IDS_MAXIMPL + 1]
short	center[3]
# magnification, and "zoom center"
data	temp /0,IIS_XCEN,IIS_YCEN_INV, 0/
# center in GKI x=256  y=255
data	center/ 16384, 16320, 0/

include "iis.com"

begin
	total = n/3

	if ( rw != IDS_WRITE) {
	    # hardware is write only
	    do i = 1, total {
		index = (i-1) * 3 + 1
		pl = frames[i]
		if ( pl == IDS_EOD)
		    break
		data[index] = zoom[pl]
		data[index+1] = xscroll[pl] * MCXSCALE
		data[index+2] = yscroll[pl] * MCYSCALE
	    }
	    if ( 3*total < n)
		data[index+3] = IDS_EOD
	    return
	}

	# can't have in data statements as IDS_EOD == (-2) and
	# fortran won't allow () in data statements!!!

	temp[4] = IDS_EOD
	center[3] = IDS_EOD
        command = IWRITE+VRETRACE
	x = ADVXONTC

	# the model 70 zooms all frames together.  So ignore "frames"
	# argument here, though needed for subsequent scroll.

	zm = data[1]
	if ( zm <= 1 )
	    zm = 0
	else if (zm >= 8)
	    zm = 3
	else
	    switch(zm) {
		case 2,3:
		    zm = 1
		
		case 4,5,6,7:
		    zm = 2
	    }
	call amovks(short(2**zm), zoom, 16)
	temp[1] = zm
	call iishdr (command, 3, ZOOM, x, 0, 0, 0)
	call iisio (temp, 3 * SZB_CHAR)

	# now we have to scroll to the desired location (in GKI).
	# If zoom is zero, don't do anything: this will leave the
	# various images panned to some previously set place, but
	# that is what is wanted when doing split screen and we pan
	# some of the images.

	if (zm != 0) {
	    do i = 1, total
	        call amovs (data[i * 3 - 1 ], scroll[i*2-1], 2)
	    scroll[total*2+1] = IDS_EOD
	    call iisscroll(short(IDS_WRITE), frames, short(total*2+1), scroll)
	}
end

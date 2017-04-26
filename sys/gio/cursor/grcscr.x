# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GRC_SCRTONDC -- Coordinate transformation from screen coordinates to NDC
# coordinates.  Screen coordinates physically address the device screen and
# range from 0 to 1 in either axis.  NDC coordinates also range from 0 to 1
# in either axis but differ from screen coordinates when the workstation
# transformation is non unitary.  The workstation transformation parameters
# are cached in the GTR common.  We assume that GTR_INIT has already been
# called to initialize the common for a graphics stream.

procedure grc_scrtondc (sx, sy, mx, my)

real	sx, sy			# screen coordinates (input)
real	mx, my			# NDC coordinates (output)
include	"gtr.com"

begin
	if (wstranset == YES) {
	    mx = ((sx * GKI_MAXNDC - xorigin) / xscale + mx1) / GKI_MAXNDC
	    my = ((sy * GKI_MAXNDC - yorigin) / yscale + my1) / GKI_MAXNDC
	} else {
	    mx = sx
	    my = sy
	}
end
 

# GRC_NDCTOSCR -- Coordinate transformation from NDC coordinates to screen
# coordinates.

procedure grc_ndctoscr (mx, my, sx, sy)

real	mx, my			# NDC coordinates (input)
real	sx, sy			# screen coordinates (output)
include	"gtr.com"

begin
	if (wstranset == YES) {
	    sx = ((mx * GKI_MAXNDC - mx1) * xscale + xorigin) / GKI_MAXNDC
	    sy = ((my * GKI_MAXNDC - my1) * yscale + yorigin) / GKI_MAXNDC
	} else {
	    sx = mx
	    sy = my
	}
end

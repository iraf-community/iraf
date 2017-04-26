# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imd.h"

# IMD_FILLAREA -- Fill a closed area.

procedure imd_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"imd.com"

begin
	# Not implemented yet.
	call imd_polyline (p, npts)
end

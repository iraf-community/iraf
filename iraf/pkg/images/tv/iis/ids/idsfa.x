# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../lib/ids.h"

# IDS_FILLAREA -- Fill a closed area.

procedure ids_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"../lib/ids.com"

begin
	# Not implemented yet.
	call ids_polyline (p, npts)
end

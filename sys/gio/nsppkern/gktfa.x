# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gkt.h"

# GKT_FILLAREA -- Fill a closed area.

procedure gkt_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"gkt.com"

begin
	# Not implemented yet.
	call gkt_polyline (p, npts)
end

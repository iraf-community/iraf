# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_FILLAREA -- Fill a closed area.

procedure stg_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"stdgraph.com"

begin
	# Not implemented yet.
	call stg_polyline (p, npts)
end

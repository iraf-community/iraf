# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccp.h"

# CCP_FILLAREA -- Fill a closed area.

procedure ccp_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"ccp.com"

begin
	# Not implemented yet.
	call ccp_polyline (p, npts)
end

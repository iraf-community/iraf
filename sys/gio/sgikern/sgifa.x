# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"sgi.h"

# SGI_FILLAREA -- Fill a closed area.

procedure sgi_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
include	"sgi.com"

begin
	# Not implemented yet.
	call sgi_polyline (p, npts)
end

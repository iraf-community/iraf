# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GPM -- Polymarker.  Draw marks of type "gk_marker" and size 2.0
# at the given positions.  Marker type has already been set.

procedure gpm (n, px, py)

int	n		# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

int	i
real	size
include	"gks.com"

begin
	# Marker size is a constant.
	size = 2.0
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
    	        call gpmark (gp[i], px, py, n, gk_marker, size, size)
	}
end

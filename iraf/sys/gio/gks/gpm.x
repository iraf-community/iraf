# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GPM -- Polymarker.  Draw marks of type "gk_marker" and size 2.0
# at the given positions.  Marker type has already been set.

procedure gpm (n, px, py)

size_t	n		# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

size_t	sz_val
int	i
real	size
include	"gks.com"

begin
	# Marker size is a constant.
	size = 2.0
	sz_val = n
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
    	        call gpmark (gp[i], px, py, sz_val, gk_marker, size, size)
	}
end

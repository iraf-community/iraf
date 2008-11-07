# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GPL -- Polyline.  Draw a line connecting the points.

procedure gpl (n, px, py)

int	n	 	# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

size_t	sz_val
int	i
include	"gks.com"

begin
	sz_val = n
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gpline (gp[i], px, py, sz_val)
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GFA -- Fill area.  The style of fill has already been set and is read
# from gio.com.

procedure gfa (n, px, py)

int	n		# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

int	i
include "gks.com"

begin
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gfill (gp[i], px, py, n, gk_style)
	}
end

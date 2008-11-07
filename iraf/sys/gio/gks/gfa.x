# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GFA -- Fill area.  The style of fill has already been set and is read
# from gio.com.

procedure gfa (n, px, py)

int	n		# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

size_t	sz_val
int	i
include "gks.com"

begin
	sz_val = n
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gfill (gp[i], px, py, sz_val, gk_style)
	}
end

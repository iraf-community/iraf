# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GPL -- Polyline.  Draw a line connecting the points.

procedure gpl (n, px, py)

size_t	n	 	# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

int	i
include	"gks.com"

begin
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gpline (gp[i], px, py, n)
	}
end

# 32-bit version of gpl for FORTRAN

procedure gpl32 (n, px, py)

int	n		# Number of points
real	px[n], py[n]	# Coordinates of points in world coordinates

size_t	sz_val

begin
	sz_val = n
	call gpl(sz_val, px, py)
end

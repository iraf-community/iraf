# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSVP -- Set viewport.  This procedure sets the viewport for world coord
# sys "wcs", which may not be the current WCS.

procedure gsvp (wcs, x1, x2, y1, y2)

int	wcs		# Number of world coordinate system
real	x1, x2		# Range of viewport coordinate in x (NDC)
real	y1, y2		# Range of viewport coordinate in y (NDC)

int	current_wcs, i
int	gstati()
include	"gks.com"

begin
	current_wcs = gstati (gp[gk_std], G_WCS)
	call gsawi (G_WCS, wcs)

	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gsview (gp[i], x1, x2, y1, y2)
	}

	# Now return to the current WCS
	call gsawi (G_WCS, current_wcs)
end

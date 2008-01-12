# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSWN -- Set window.  Window of world coord system "wcs" is set, which
# is not necessarily the current WCS.

procedure gswn (wcs, x1, x2, y1, y2)

int	wcs		# Number of world coordinate system (transformation)
real	x1, x2		# Range of world coordinates in x
real	y1, y2		# Range of world coordinates in y

int	current_wcs, i
int	gstati()
include	"gks.com"

begin
	current_wcs = gstati (gp[gk_std], G_WCS)
	call gsawi (G_WCS, wcs)
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
	    call gswind (gp[i], x1, x2, y1, y2)
	}

	# Now return to current WCS before returning
	call gsawi (G_WCS, current_wcs)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"grc.h"

# GRC_REDRAW -- Redraw the screen, and, if the "axes" flag is set, draw the axes
# of the plot.

procedure grc_redraw (rc, stream, sx, sy, raster, rx, ry)

pointer	rc			#I rcursor descriptor
int	stream			#I graphics stream
real	sx, sy			#I screen coords of cursor
int	raster			#I raster number
real	rx, ry			#I raster coords of cursor

begin
	call gtr_redraw (stream)
	if (RC_AXES(rc) == YES)
	    call grc_axes (stream, sx, sy, raster, rx, ry)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

# GLB_DRAWGRID -- Draw a grid across the plotting surface, i.e., draw
# dotted lines between the major tick marks.

procedure glb_drawgrid (gp, ax1, ax2)

pointer	gp			# graphics descriptor
pointer	ax1			# descriptor for first axis
pointer	ax2			# descriptor for second axis

int	wcs, major_tick
real	x, y, tolerance
real	x1, y1, x2, y2, sx, sy
int	glb_gettick()
errchk	glb_gettick, gseti, gsetr, gline, gctran

begin
	tolerance = TOL
	wcs = GP_WCS(gp)

	# Cache the NDC coordinates of the ends of an axis.
	call gctran (gp, AX_START(ax1,1), AX_START(ax1,2), x1,y1, wcs, 0)
	call gctran (gp,   AX_END(ax1,1),   AX_END(ax1,2), x2,y2, wcs, 0)

	# Set polyline linetype for a dotted line.
	call gseti (gp, G_PLTYPE, GL_DOTTED)
	call gsetr (gp, G_PLWIDTH, 1.0)

	AX_NLEFT(ax1) = -1
	while (glb_gettick (gp, ax1, x, y, major_tick) != EOF) {
	    if (major_tick == NO)
		next

	    # Draw grid line if we are at a major tick, provided the tick
	    # is not at the end of the axis.

	    call gctran (gp, x,y, sx,sy, wcs, 0)
	    if (AX_HORIZONTAL(ax1) == YES) {
		if (sx - x1 > tolerance && sx - x2 < tolerance)
		    call gline (gp, x, AX_END(ax1,2), x, AX_END(ax2,2))
	    } else {
		if (sy - y1 > tolerance && sy - y2 < tolerance)
		    call gline (gp, AX_END(ax1,1), y, AX_END(ax2,1), y)
	    }
	}

	call gseti (gp, G_PLTYPE, GL_SOLID)
end

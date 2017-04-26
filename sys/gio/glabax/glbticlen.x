# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	"glabax.h"

# GLB_TICKLEN -- Compute the length of a tick in world coordinates.  All tick
# drawing is performed in world coordinates since the ticks show the world
# coordinate system.  The position of a tick must be computed in world coords
# when the axis is drawn to reflect log scaling (or any other nonlinear
# scaling.  Less obviously, the tick offset should be given in world coords
# so that when the tick is drawn by a GRDRAW the tick will follow a line of
# constant X or Y in world coordinates, and this line will not necessarily be
# a line of constant X or Y in NDC coordinates.

real procedure glb_ticklen (gp, ax, ndc_length)

pointer	gp			# graphics descriptor
pointer	ax			# axis descriptor
real	ndc_length		# length of tick in NDC units

int	wcs
real	x, y, wx, wy

begin
	wcs = GP_WCS(gp)
	call gctran (gp, AX_TICK1(ax,1), AX_TICK1(ax,2), x, y, wcs, 0)

	if (AX_HORIZONTAL(ax) == YES)
	    y = y + ndc_length
	else
	    x = x + ndc_length

	call gctran (gp, x, y, wx, wy, 0, wcs)
	if (AX_HORIZONTAL(ax) == YES) {
	    call pargr (wy - AX_TICK1(ax,2))
	    return (wy - AX_TICK1(ax,2))
	} else {
	    call pargr (wx - AX_TICK1(ax,1))
	    return (wx - AX_TICK1(ax,1))
	}
end

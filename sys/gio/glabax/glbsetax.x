# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	"glabax.h"

# GLB_SET_AXES -- Set all axis descriptor parameters not pertaining to the
# ticks.  The WCS has already been fixed by the time we get here.

procedure glb_set_axes (gp, ap, ax1, ax2, angle)

pointer	gp			# graphics descriptor
pointer	ap			# axis parameters (from graphics descriptor)
pointer	ax1, ax2		# axis descriptors (output)
int	angle			# axis orientation, 0 or 90 degrees

pointer	w
int	axis
real	p1, p2
real	x1, x2, y1, y2
real	glb_ticklen()

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))

	# If the window was rounded in Y in the second call to find_ticks,
	# then the Y positions of the first ticks set in the first call will
	# be in error and must be corrected.  If the user has elected to set
	# the axis position explicitly, however, then we must leave it alone.

	if (angle == 0 && GL_SETAXISPOS(GP_XAP(gp)) == NO) {
	    AX_TICK1(ax1,2) = WCS_WY1(w)
	    AX_TICK1(ax2,2) = WCS_WY2(w)
	}

	# Set the tick lengths.  This is done here rather than in findticks
	# due to rounding, as noted above.  The tick offsets in world
	# coordinates.  The GL values are given in NDC coordinates.

	if (angle == 0) {
	    axis = 2
	    AX_HORIZONTAL(ax1) = YES
	    AX_HORIZONTAL(ax2) = YES
	} else {
	    axis = 1
	    AX_HORIZONTAL(ax1) = NO
	    AX_HORIZONTAL(ax2) = NO
	}

	AX_MAJORTICK(ax1,axis) = glb_ticklen (gp, ax1,  GL_MAJORLENGTH(ap))
	AX_MINORTICK(ax1,axis) = glb_ticklen (gp, ax1,  GL_MINORLENGTH(ap))
	AX_MAJORTICK(ax2,axis) = glb_ticklen (gp, ax2, -GL_MAJORLENGTH(ap))
	AX_MINORTICK(ax2,axis) = glb_ticklen (gp, ax2, -GL_MINORLENGTH(ap))

	# Select none, either, or both axes to be drawn.  If only the second
	# axis is drawn then that is the side we must draw the tick and axis
	# labels on.

	switch (GL_DRAWAXES(ap)) {
	case 0:
	    AX_DRAWME(ax1) = NO
	    AX_DRAWME(ax2) = NO
	    return
	case 1:
	    AX_DRAWME(ax1) = YES
	    AX_DRAWME(ax2) = NO
	case 2:
	    AX_DRAWME(ax1) = NO
	    AX_DRAWME(ax2) = YES
	default:
	    AX_DRAWME(ax1) = YES
	    AX_DRAWME(ax2) = YES
	}

	# Determine the endpoints of the axis.  These default to the corners of
	# the viewport (in world coordinates), but the positions may be
	# overriden by the user if desired.

	# First get the positions of the two axes.
	if (GL_SETAXISPOS(ap) == YES) {
	    p1 = GL_AXISPOS1(ap)
	    p2 = GL_AXISPOS2(ap)
	} else if (angle == 0) {
	    p1 = WCS_WY1(w)
	    p2 = WCS_WY2(w)
	} else {
	    p1 = WCS_WX1(w)
	    p2 = WCS_WX2(w)
	}

	# Convert these positions into the world coordinates of the endpoints.
	if (angle == 0) {
	    x1 = WCS_WX1(w)
	    x2 = WCS_WX2(w)
	    y1 = p1
	    y2 = p2
	} else {
	    x1 = p1
	    x2 = p2
	    y1 = WCS_WY1(w)
	    y2 = WCS_WY2(w)
	}

	if (angle == 0) {
	    # Set the left and right endpoints of the axes.

	    AX_START(ax1,1) = x1
	    AX_START(ax1,2) = y1
	    AX_END(ax1,1)   = x2
	    AX_END(ax1,2)   = y1

	    AX_START(ax2,1) = x1
	    AX_START(ax2,2) = y2
	    AX_END(ax2,1)   = x2
	    AX_END(ax2,2)   = y2

	} else {
	    # Set the lower and upper endpoints of the axes.

	    AX_START(ax1,1) = x1
	    AX_START(ax1,2) = y1
	    AX_END(ax1,1)   = x1
	    AX_END(ax1,2)   = y2

	    AX_START(ax2,1) = x2
	    AX_START(ax2,2) = y1
	    AX_END(ax2,1)   = x2
	    AX_END(ax2,2)   = y2
	}
end

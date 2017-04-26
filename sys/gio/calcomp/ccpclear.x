# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ccp.h"

# CCP_CLEAR -- Advance a frame on the plotter.  All attribute packets are
# initialized to their default values.  Redundant calls or calls immediately
# after a workstation open (before anything has been drawn) are ignored.

procedure ccp_clear (dummy)

int	dummy			# not used at present
include	"ccp.com"

begin
	# This is a no-op if nothing has been drawn.
	if (g_cc == NULL || g_ndraw == 0)
	    return

	# Start a new frame.  This is by resetting the origin to the last
	# x-position drawn plus a compile-time offset.

	call plot (g_max_x + FRAME_OFFSET, 0.0, -3)
	g_max_x = 0.0

	# Init kernel data structures.
	call ccp_reset()
	g_ndraw = 0
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"imd.h"

# IMD_CLEAR -- Advance a frame on the plotter.  All attribute packets are
# initialized to their default values.  Redundant calls or calls immediately
# after a workstation open (before anything has been drawn) are ignored.

procedure imd_clear (dummy)

int	dummy			# not used at present

int	idk_open()
errchk	idk_open
include	"imd.com"

begin
	# This is a no-op if nothing has been drawn.
	if (g_kt == NULL || g_ndraw == 0)
	    return

	# Start a new frame.  This is done either by issuing the frame advance
	# instruction or by starting a new metafile.  Close the output file and 
	# start a new metafile if the maximum frame count has been reached.
	# This disposes of the metafile to the system, causing the actual
	# plots to be drawn.  Open a new metafile ready to receive next frame.

	g_nframes = g_nframes + 1
	if (g_nframes >= g_maxframes) {

	    # Does this device require a frame advance at end of metafile?
	    if (IMD_ENDFRAME(g_kt) == YES)
		call idk_frame (g_out)

	    g_nframes = 0
	    call idk_close (g_out)
	    #g_out = idk_open (Memc[IMD_DEVNAME(g_kt)], g_tty)
	    g_out = idk_open (g_frame, g_color, g_tty)

	    # Does this device require a frame advance at beginning of metafile?
	    if (IMD_STARTFRAME(g_kt) == YES)
		call idk_frame (g_out)

	} else {
	    # Merely output frame instruction to start a new frame in the same
	    # metafile.

	    call idk_frame (g_out)
	}

	# Init kernel data structures.
	call imd_reset()
	g_ndraw = 0
end

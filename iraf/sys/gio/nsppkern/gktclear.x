# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"gkt.h"

# GKT_CLEAR -- Advance a frame on the plotter.  All attribute packets are
# initialized to their default values.  Redundant calls or calls immediately
# after a workstation open (before anything has been drawn) are ignored.

procedure gkt_clear (dummy)

int	dummy			# not used at present

int	gkt_mfopen()
errchk	gkt_mfopen
include	"gkt.com"

begin
	# This is a no-op if nothing has been drawn.
	if (g_kt == NULL || g_ndraw == 0)
	    return

	# Start a new frame.  This is done either by calling NSPP to do a frame
	# advance or by starting a new metafile.  Close the output file and 
	# start a new metafile if the maximum frame count has been reached.
	# This disposes of the metafile to the system, causing the actual
	# plots to be drawn.  Open a new metafile ready to receive next frame.

	g_nframes = g_nframes + 1
	if (g_nframes >= g_maxframes) {

	    # Does this device require a frame advance at end of metafile?
	    if (GKT_ENDFRAME(g_kt) == YES)
		call frame()

	    # The call to the NSPP flush procedure must be escaped to avoid
	    # interpretation as the FIO flush procedure.

%	    call mcflsh

	    g_nframes = 0
	    call close (g_out)

	    g_out = gkt_mfopen (g_tty, NEW_FILE)

	    # Does this device require a frame advance at beginning of metafile?
	    if (GKT_STARTFRAME(g_kt) == YES)
		call frame()

	} else {
	    # Merely output NSPP frame instruction to start a new frame in
	    # the same metafile.

	    call frame()
	}

	# Init kernel data structures.
	call gkt_reset()
	g_ndraw = 0
end

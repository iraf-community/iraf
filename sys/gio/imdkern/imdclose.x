# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imd.h"

# IMD_CLOSE -- Close the IMD translation kernel.  Close the spool file so
# the output is finally plotted.  Free up storage.

procedure imd_close()

include	"imd.com"

begin
	# Check for a redundant imd_close call.
	if (g_kt == NULL)
	    return

	# If there is anything in the metafile, flush it and add a frame
	# advance if required for the device.

	if (g_ndraw > 0 || g_nframes > 0) {
	    # Does this device require a frame advance at end of metafile?
	    if (IMD_ENDFRAME(g_kt) == YES)
		call idk_frame (g_out)
	}

	# Close output metafile, disposing of it to the host system.
	call idk_close (g_out)

	# Return tty descriptor.
	call ttycdes (g_tty)

	# Free kernel data structures.
	call mfree (IMD_SBUF(g_kt), TY_CHAR)
	call mfree (g_kt, TY_STRUCT)

	g_kt = NULL
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"sgi.h"

# SGI_CLOSE -- Close the SGI translation kernel.  Close the spool file so
# the output is finally plotted.  Free up storage.

procedure sgi_close()

include	"sgi.com"

begin
	# If there is anything in the metafile, flush it and add a frame
	# advance if required for the device.

	if (g_ndraw > 0 || g_nframes > 0) {
	    # Does this device require a frame advance at end of metafile?
	    if (SGI_ENDFRAME(g_kt) == YES)
		call sgk_frame (g_out)
	}

	# Close output metafile, disposing of it to the host system.
	call sgk_close (g_out)

	# Free kernel data structures.
	call mfree (SGI_SBUF(g_kt), TY_CHAR)
	call mfree (g_kt, TY_STRUCT)

	g_kt = NULL
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gkt.h"

# GKT_CLOSE -- Close the nspp translation kernel.  Close the spool file so
# the output is finally plotted.  Free up storage.

procedure gkt_close()

include	"gkt.com"

begin
	# If there is anything in the metafile, flush it and add a frame
	# advance if required for the device.

	if (g_ndraw > 0 || g_nframes > 0) {
	    # Does this device require a frame advance at end of metafile?
	    if (GKT_ENDFRAME(g_kt) == YES)
		call frame()

	    # The call to the NSPP flush procedure must be escaped to avoid
	    # interpretation as the FIO flush procedure.

%	    call mcflsh
	}

	# Close output metafile, disposing of it to the host system.
	call close (g_out)

	# Free kernel data structures.
	call mfree (GKT_SBUF(g_kt), TY_CHAR)
	call mfree (g_kt, TY_STRUCT)

	g_kt = NULL
end

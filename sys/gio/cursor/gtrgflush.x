# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gio.h>
include	"gtr.h"

# GTR_GFLUSH -- Dispose of any buffered output on the stream STDPLOT.  The last
# plot sent to stdplot cannot be disposed of at CLOSEWS time due to the need
# to permit APPEND mode in the next OPENWS call.  We are called to dispose
# of all output to the plotter device.  Logging out or doing a reset will have
# the same effect.

procedure gtr_gflush (stream)

int	stream
pointer	tr
bool	streq()
include	"gtr.com"

begin
	tr = trdes[stream]
	if (tr == NULL)
	    return

	# Disconnect the kernel.
	iferr {
	    if (streq (TR_KERNFNAME(tr), "cl"))
		call stg_close()
	    else if (TR_DEVNAME(tr) != EOS && TR_KERNFNAME(tr) != EOS) {
		call gtr_disconnect (TR_PID(tr), TR_IN(tr), TR_OUT(tr),
		    stream)
		TR_PID(tr) = NULL
	    }
	} then
	    call erract (EA_WARN)

	# Free all storage.
	call mfree (TR_FRAMEBUF(tr), TY_SHORT)
	call mfree (TR_SCRATCHBUF(tr), TY_SHORT)
	call mfree (tr, TY_STRUCT)

	trdes[stream] = NULL
	if (tr_stream == stream)
	    tr_stream = NULL
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gio.h>
include	"gtr.h"

# GTR_RESET -- Reset the graphics system.  Disconnect all connected subkernels
# and free all file descriptors and memory.

procedure gtr_reset (status)

int	status				# not used (req. for ONEXIT)

pointer	tr
int	stream
bool	streq()
include	"gtr.com"

begin
	do stream = STDGRAPH, STDPLOT {
	    tr = trdes[stream]
	    if (tr == NULL)
		next

	    iferr {
		# Close device graphcap descriptor.
		if (TR_TTY(tr) != NULL)
		    call ttycdes (TR_TTY(tr))

		# Disconnect old kernel.
		if (streq (TR_KERNFNAME(tr), "cl"))
		    call stg_close()
		else if (TR_DEVNAME(tr) != EOS && TR_KERNFNAME(tr) != EOS) {
		    call gtr_disconnect (TR_PID(tr),
			TR_IN(tr), TR_OUT(tr), stream)
		    TR_PID(tr) = NULL
		    TR_IN(tr) = NULL
		    TR_OUT(tr) = NULL
		}
	    } then {
		TR_DEVNAME(tr) = EOS
		call erract (EA_WARN)
	    } else
		TR_DEVNAME(tr) = EOS

	    # Free all storage.
	    call mfree (TR_FRAMEBUF(tr), TY_SHORT)
	    call mfree (TR_SCRATCHBUF(tr), TY_SHORT)
	    call mfree (tr, TY_STRUCT)

	    trdes[stream] = NULL
	}
end

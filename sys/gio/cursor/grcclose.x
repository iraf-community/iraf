# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"gtr.h"
include	"grc.h"

# GRC_CLOSE -- Close the workstation (kernel).  Called by RCURSOR to close the
# kernel after a cursor read.  Note that a cursor read may occur while the
# workstation is open, i.e., after gopen but before gclose, or after the
# workstation has been closed, i.e., after a plotting program terminates.
# If the workstation was already open (GKI_OPENWS) by the application when
# the cursor read occurred we must leave things as they were.

procedure grc_close (fd, rc)

int	fd			# graphics stream
pointer	rc			# rcursor descriptor

pointer	tr
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (fd)

	# Decrement the logical OPENWS count and issue the actual CLOSEWS
	# only if the counter goes to zero.  If the workstation was open
	# but deactivated when grc_open() was called (WS_ACTIVE == NO),
	# restore it to its former (deactivated) state.

	TR_WSOPEN(tr) = TR_WSOPEN(tr) - 1
	if (TR_WSOPEN(tr) <= 0) {
	    call gki_closews (fd, TR_DEVNAME(tr))
	    TR_WSOPEN(tr) = 0
	    TR_WSACTIVE(tr) = NO
	} else if (TR_WSACTSAVE(tr) == NO) {
	    call gki_deactivatews (fd, 0)
	    TR_WSACTIVE(tr) = NO
	}

	call gki_fflush (fd)
end

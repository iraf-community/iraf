# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include "iis.h"

define	LEN_HID	5

# IISCLS -- Close IIS display.

procedure iiscls (chan, status)

int	chan[ARB]
int	status

include	"iis.com"

begin
	# first we need to tuck away the constants for zoom and scroll
	# as we cannot read them on the model 70.  Would that there were
	# somewhere to put them.  Alas not.  So just drop them on the floor.

	if (iisnopen == 1) {
	    call zclsgd (iischan, status)
	    iisnopen = 0
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "zdisplay.h"
include "iis.h"

# IISCLS -- Close IIS display.

procedure iiscls (chan, status)

int	chan[ARB]
int	status
include	"iis.com"

begin
	if (iisnopen == 1) {
	    call zclsgd (iischan, status)
	    iisnopen = 0
	} else if (iisnopen > 1) {
	    iisnopen = iisnopen - 1
	} else
	    iisnopen = 0
end

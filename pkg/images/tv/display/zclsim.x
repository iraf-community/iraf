# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZCLSIM -- Close an image display frame which is addressable as
# a binary file.

procedure zclsim (chan, status)

int	chan[ARB]
int	status
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iiscls (chan, status)
	default:
	    status = ERR
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZCLRIM -- Color window binary file display device.

procedure zclrim (chan)

int	chan[ARB]
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisclr (chan)
	}
end

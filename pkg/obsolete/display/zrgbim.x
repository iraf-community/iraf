# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZRGBIM -- Set RGB display.

procedure zrgbim (red_chan, green_chan, blue_chan)

int	red_chan[ARB], green_chan[ARB], blue_chan[ARB]

int	device

begin
	device = red_chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisrgb (red_chan, green_chan, blue_chan)
	}
end

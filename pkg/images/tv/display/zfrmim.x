# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZFRMIM -- Set FRAME display.

procedure zfrmim (chan)

int	chan[ARB]

int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisrgb (chan, chan, chan)
	}
end

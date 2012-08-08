# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZRMIM -- Zoom and roam display.

procedure zrmim (chan, zfactor)

int	chan[ARB]
int	zfactor
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisrm (zfactor)
	}
end

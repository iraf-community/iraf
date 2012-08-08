# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZMTCIM -- Match lut to frame.

procedure zmtcim (chan1, chan2)

int	chan1[ARB], chan2[ARB]
int	device

begin
	device = chan1[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iismtc (chan1, chan2)
	}
end

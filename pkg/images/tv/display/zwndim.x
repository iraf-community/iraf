# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZWNDIM -- Window binary file display device.

procedure zwndim (chan)

int	chan[ARB]
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iiswnd3 (chan, chan, chan)
	}
end

procedure zwndim3 (chan1, chan2, chan3)

int	chan1[ARB], chan2[ARB], chan3[ARB]
int	device

begin
	device = chan1[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iiswnd3 (chan1, chan2, chan3)
	}
end

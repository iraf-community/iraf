# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZAWTIM -- Wait for an image display frame which is addressable as
# a binary file.

procedure zawtim (chan, nbytes)

int	chan[ARB], nbytes
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iiswt (chan, nbytes)
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZARDIM -- Read data from a binary file display device.

procedure zardim (chan, buf, nbytes, offset)

int	chan[ARB]
short	buf[ARB]
int	nbytes
long	offset
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisrd (chan, buf, nbytes, offset)
	}
end

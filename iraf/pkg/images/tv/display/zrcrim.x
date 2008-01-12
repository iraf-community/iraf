# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZRCRIM -- Read Cursor from binary file display device.

procedure zrcrim (chan, xcur, ycur)

int	chan[ARB]
int	status, xcur, ycur
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisrcr (status, xcur, ycur)
	}
end

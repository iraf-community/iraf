# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZMAPIM -- Set display map.

procedure zmapim (chan, maptype)

int	chan[ARB]
char	maptype[ARB]
int	device

begin
	device = chan[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisofm (maptype)
	}
end

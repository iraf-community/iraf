# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"iis.h"

# ZARDIM -- Read data from a binary file display device.

procedure zardim (chan, buf, nbytes, offset)

int	chan[ARB]
short	buf[ARB]
int	nbytes
long	offset

begin
	    call iisrd (chan, buf, nbytes, offset)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <knet.h>

# ZSTTIM -- Return status on binary file display device.

procedure zsttim (chan, what, lvalue)

int	chan[ARB], what
long	lvalue

begin
	call zsttgd (chan, what, lvalue)
end

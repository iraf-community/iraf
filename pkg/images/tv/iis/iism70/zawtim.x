# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	"iis.h"

# ZAWTIM -- Wait for an image display frame which is addressable as
# a binary file.

procedure zawtim (chan, nbytes)

int	chan[ARB], nbytes
include	"iis.com"

begin
	call iiswt (chan, nbytes)
end

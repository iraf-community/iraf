# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ids.h"

# IDS_CLOSEWS -- Close the named workstation.

procedure ids_closews (devname, n)

short	devname[n]		# device name (not used)
int	n			# length of device name
include "ids.com"

begin
	call ids_flush(0)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gkt.h"

# GKT_CLOSEWS -- Close the named workstation.  Flush the output.
# The spool file is closed only on the next plot or at gktclose time.
# If the spool file is closed here, APPEND mode would not work.

procedure gkt_closews (devname, n)

short	devname[ARB]		# device name (not used)
int	n			# length of device name
include "gkt.com"

begin
	call gkt_flush (0)
end

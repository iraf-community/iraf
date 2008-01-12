# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"sgi.h"

# SGI_CLOSEWS -- Close the named workstation.  Flush the output.
# The spool file is closed only on the next plot or at gktclose time.
# If the spool file is closed here, APPEND mode would not work.

procedure sgi_closews (devname, n)

short	devname[ARB]		# device name (not used)
int	n			# length of device name
include "sgi.com"

begin
	call sgk_flush (g_out)
end

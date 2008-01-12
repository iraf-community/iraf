# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imd.h"

# IMD_CLOSEWS -- Close the named workstation.  Flush the output.
# The spool file is closed only on the next plot or at gktclose time.
# If the spool file is closed here, APPEND mode would not work.

procedure imd_closews (devname, n)

short	devname[ARB]		# device name (not used)
int	n			# length of device name
include "imd.com"

begin
	# For the IMD kernel, all display graphics writes are in append mode,
	# so we may as well shutdown completely for closews (this also ensures
	# that the display is updated at closews time).

	#call idk_flush (g_out)
	call imd_close()
end

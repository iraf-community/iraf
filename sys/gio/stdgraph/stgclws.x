# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttset.h>
include	"stdgraph.h"

# STG_CLOSEWS -- Close the named workstation.  Output the termination string,
# if any, and flush the output.  Buffer deallocation is handled by STGCLOSE.

procedure stg_closews (devname, n)

short	devname[ARB]		# device name (not used)
int	n			# length of device name

include "stdgraph.com"

begin
	call stg_ctrl ("CW")
	call flush (g_out)

	g_active = NO
	g_enable = NO

	# Reenable stty ucaseout mode if it was set when the workstation
	# was activated.

	if (g_ucaseout == YES)
	    call ttseti (g_out, TT_UCASEOUT, YES)
end

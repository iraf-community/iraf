# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_CLEAR -- Clear the workstation screen.  All attribute packets are
# initialized to their default values when the screen is cleared.

procedure stg_clear (dummy)

int	dummy			# not used at present
include	"stdgraph.com"

begin
	call stg_ctrl ("CL")
	call stg_reset()
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_GDISAB -- Disable graphics, i.e., issue the GD control sequence.

procedure stg_gdisab()

include	"stdgraph.com"

begin
	if (g_active == YES && g_out > 0) {
	    call stgctrl ("GD")
	    call flush (g_out)
	    g_enable = NO
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_GENAB -- Enable graphics, i.e., issue the GE control sequence.

procedure stg_genab()

include	"stdgraph.com"

begin
	if (g_active == YES && g_out > 0) {
	    call stgctrl ("GE")
	    call flush (g_out)
	    g_enable = YES
	}
end

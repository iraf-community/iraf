# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_FLUSH -- Flush output.

procedure stg_flush (dummy)

int	dummy			# not used at present
include	"stdgraph.com"

begin
	call flush (g_out)
end

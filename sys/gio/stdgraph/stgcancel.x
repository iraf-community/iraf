# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	"stdgraph.h"

# STG_CANCEL -- Cancel any buffered output.

procedure stg_cancel (dummy)

int	dummy			# not used at present
include	"stdgraph.com"

begin
	call fseti (g_out, F_CANCEL, YES)
	call stg_reset()
end

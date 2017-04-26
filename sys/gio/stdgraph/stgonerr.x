# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_ONERROR -- Called when error recovery takes place to deactivate the
# stdgraph workstation, i.e., take the terminal out of graphics mode.  If
# this is not done error messages will be written as vectors.

procedure stg_onerror (errcode)

int	errcode
include	"stdgraph.com"

begin
	if (g_active == YES)
	    call stg_deactivatews (0)
end

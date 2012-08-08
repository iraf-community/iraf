# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_GRSTREAM -- Set the FD of the graphics stream, from which we shall read
# metacode instructions and to which we shall return cell arrays and cursor
# values.

procedure stg_grstream (stream)

int	stream			# FD of the new graphics stream
include	"stdgraph.com"

begin
	g_stream = stream
end

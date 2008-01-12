# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imd.h"

# IMD_FLUSH -- Flush output.

procedure imd_flush (dummy)

int	dummy			# not used at present
include	"imd.com"

begin
	call idk_flush (g_out)
end

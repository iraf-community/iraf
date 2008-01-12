# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imd.h"

# IMD_CANCEL -- Cancel any buffered output.

procedure imd_cancel (dummy)

int	dummy			# not used at present
include	"imd.com"

begin
	if (g_kt == NULL)
	    return
	call imd_reset()
end

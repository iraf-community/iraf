# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"sgi.h"

# SGI_CANCEL -- Cancel any buffered output.

procedure sgi_cancel (dummy)

int	dummy			# not used at present
include	"sgi.com"

begin
	if (g_kt == NULL)
	    return
	call sgi_reset()
end

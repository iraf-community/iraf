# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"sgi.h"

# SGI_FLUSH -- Flush output.

procedure sgi_flush (dummy)

int	dummy			# not used at present
include	"sgi.com"

begin
	call sgk_flush (g_out)
end

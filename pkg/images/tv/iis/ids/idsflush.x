# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../lib/ids.h"

# IDS_FLUSH -- Flush output.

procedure ids_flush (dummy)

int	dummy			# not used at present
include	"../lib/ids.com"

begin
	if (i_kt == NULL)
	    return

	# We flush the FIO stream.
	call flush (i_out)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	"../lib/ids.h"

# IDS_CANCEL -- Cancel any buffered output.

procedure ids_cancel (dummy)

int	dummy			# not used at present
include	"../lib/ids.com"

begin
	if (i_kt == NULL)
	    return

	# Just cancel any output in the FIO stream
	call fseti (i_out, F_CANCEL, OK)
end

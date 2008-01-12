# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	"gkt.h"

# GKT_CANCEL -- Cancel any buffered output.

procedure gkt_cancel (dummy)

int	dummy			# not used at present
include	"gkt.com"

begin
	if (g_kt == NULL)
	    return

	# First we cancel any output in the FIO stream, then
	# flush the nspp buffers.  This might, of course,
	# put something in the FIO stream, so we cancel again.
	# note the Fortran escape for "flush"...spp has a reserved
	# word of the same name.

	call fseti (g_out, F_CANCEL, OK)
%	call mcflsh
	call fseti (g_out, F_CANCEL, OK)
	call gkt_reset()
end

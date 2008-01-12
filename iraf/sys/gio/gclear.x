# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gset.h>

# GCLEAR -- Clear the screen and initialize all internal state variables to
# the original GOPEN state.  Plots separated by calls to GCLEAR cannot affect
# each other.  See also GFRAME and GRESET if a full state reset is not
# desired.

procedure gclear (gp)

pointer	gp			# graphics descriptor

begin
	call gactivate (gp, 0)
	call gpl_flush()
	call gki_clear (GP_FD(gp))
	call greset (gp, GR_RESETALL)
end

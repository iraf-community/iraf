# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GFRAME -- Clear the screen, but do not modify the internal state of GIO,
# other than to reset the WCS and attribute packet states to UNSET, to force
# retranmission to the graphics kernel.

procedure gframe (gp)

pointer	gp			# graphics descriptor

begin
	call gactivate (gp, 0)
	call gpl_flush()
	call gki_clear (GP_FD(gp))
	call gfrinit (gp)
end

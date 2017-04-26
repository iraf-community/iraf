# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GFRINIT -- Initialize the internal state variables of GIO for a new frame.
# The state of all the attribute packets is set to UNSET to force them to be
# retransmitted to the graphics kernel when i/o occurs.

procedure gfrinit (gp)

pointer gp			# graphics descriptor
pointer	ap

begin
	# Force retransmission of the WCS.
	GP_WCSSTATE(gp) = UNSET

	# Force retransmission of the attribute packets.
	ap = GP_PLAP(gp);	PL_STATE(ap) = UNSET
	ap = GP_PMAP(gp);	PM_STATE(ap) = UNSET
	ap = GP_FAAP(gp);	FA_STATE(ap) = UNSET
	ap = GP_TXAP(gp);	TX_STATE(ap) = UNSET
	ap = GP_TXAPCUR(gp);	TX_STATE(ap) = UNSET

	call gpl_reset()
end

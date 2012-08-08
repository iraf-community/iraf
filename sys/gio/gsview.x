# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GSVIEW -- Set the viewport of the current WCS.

procedure gsview (gp, x1, x2, y1, y2)

pointer	gp			# graphics descriptor
real	x1, x2			# range of NDC in X
real	y1, y2			# range of NDC in Y
pointer	w

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))

	WCS_SX1(w) = x1
	WCS_SX2(w) = x2
	WCS_SY1(w) = y1
	WCS_SY2(w) = y2

	WCS_FLAGS(w) = or (WCS_FLAGS(w), WF_DEFINED)
	GP_WCSSTATE(gp) = MODIFIED
	call gpl_reset()
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GSWIND -- Set the window into world coordinates of the current WCS.

procedure gswind (gp, x1, x2, y1, y2)

pointer	gp			# graphics descriptor
real	x1, x2			# range of world coords in X
real	y1, y2			# range of world coords in Y
pointer	w

begin
	call gpl_flush()
	w = GP_WCSPTR (gp, GP_WCS(gp))

	if (!IS_INDEF(x1))
	    WCS_WX1(w) = x1
	if (!IS_INDEF(x2))
	    WCS_WX2(w) = x2
	if (!IS_INDEF(y1))
	    WCS_WY1(w) = y1
	if (!IS_INDEF(y2))
	    WCS_WY2(w) = y2

	WCS_FLAGS(w) = or (WCS_FLAGS(w), WF_DEFINED)
	GP_WCSSTATE(gp) = MODIFIED
	call gpl_reset()
end

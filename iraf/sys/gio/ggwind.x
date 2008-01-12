# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGWIND -- Get the window into world coordinates of the current WCS.

procedure ggwind (gp, x1, x2, y1, y2)

pointer	gp			# graphics descriptor
real	x1, x2			# range of world coords in X (output)
real	y1, y2			# range of world coords in Y (output)
pointer	w

begin
	call gactivate (gp, 0)
	w = GP_WCSPTR (gp, GP_WCS(gp))

	x1 = WCS_WX1(w)
	x2 = WCS_WX2(w)
	y1 = WCS_WY1(w)
	y2 = WCS_WY2(w)
end

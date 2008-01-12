# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGVIEW -- Get the viewport of the current WCS.

procedure ggview (gp, x1, x2, y1, y2)

pointer	gp			# graphics descriptor
real	x1, x2			# range of NDC in X (output)
real	y1, y2			# range of NDC in Y (output)
pointer	w

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))

	x1 = WCS_SX1(w)
	x2 = WCS_SX2(w)
	y1 = WCS_SY1(w)
	y2 = WCS_SY2(w)
end

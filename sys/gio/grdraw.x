# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GRDRAW -- Relative draw, i.e., move the pen to the specified offset from the
# current position.

procedure grdraw (gp, x, y)

pointer	gp			# graphics descriptor
real	x, y			# offset from current position
real	cx, cy

begin
	if (IS_INDEF(x) || IS_INDEF(y))
	    call gadraw (gp, x, y)
	else {
	    call gcurpos (gp, cx, cy)
	    if (IS_INDEF(cx) || IS_INDEF(cy))
		call gadraw (gp, INDEF, INDEF)
	    else
		call gadraw (gp, cx + x, cy + y)
	}
end

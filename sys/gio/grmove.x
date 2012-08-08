# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GRMOVE -- Relative move, i.e., move the pen to the specified offset from the
# current position (without generating any output).

procedure grmove (gp, x, y)

pointer	gp			# graphics descriptor
real	x, y			# offset from current position
real	cx, cy

begin
	call gpl_flush()
	if (IS_INDEF(x) || IS_INDEF(y))
	    call gadraw (gp, x, y)
	else {
	    call gcurpos (gp, cx, cy)
	    if (!(IS_INDEF(cx) || IS_INDEF(cy)))
		call gamove (gp, cx + x, cy + y)
	}
end

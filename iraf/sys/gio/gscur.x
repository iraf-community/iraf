# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GSCUR -- Set the current graphics cursor to the position (x,y) in world
# coordinates.

procedure gscur (gp, x, y)

pointer gp			# graphics descriptor
real	x, y			# new position for cursor
real	mx, my

begin
	call gpl_flush()
	call gpl_wcstogki (gp, x, y, mx, my)
	call gki_setcursor (GP_FD(gp), nint(mx), nint(my), GP_CURSOR(gp))
end

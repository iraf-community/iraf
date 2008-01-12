# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GAMOVE -- Absolute move.  Move the pen to the indicated position in
# preparation for a draw.

procedure gamove (gp, x, y)

pointer	gp			# graphics descriptor
real	x, y			# new position of pen
include	"gpl.com"

begin
	if (op > 1)
	    call gpl_flush()

	if (IS_INDEF(x) || IS_INDEF(y)) {
	    # Set current position to indefinite.
	    cx = INDEF
	    cy = INDEF
	} else {
	    # Set current position to (x,y) in GKI coordinates.
	    call gpl_wcstogki (gp, x, y, cx, cy)
	}
end

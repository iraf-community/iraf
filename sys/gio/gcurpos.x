# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GCURPOS -- Get the current position in world coordinates.  The current
# position is maintained internally in GKI coordinates to make it invariant
# with respect to changes in the current WCS.

procedure gcurpos (gp, x, y)

pointer	gp			# graphics descriptor
real	x, y			# current position in current WCS (output)

real	aelogr()
include	"gpl.com"

begin
	if (gp != gp_out || GP_WCS(gp) != wcs)
	    call gpl_cache (gp)

	if (IS_INDEF(cx) || IS_INDEF(cy)) {
	    x = INDEF
	    y = INDEF

	} else {
	    x = (cx - mxorigin) / xscale + wxorigin
	    if (xtran != LINEAR)
		if (xtran == LOG)
		    x = 10.0 ** x
		else
		    x = aelogr (x)

	    y = (cy - myorigin) / yscale + wyorigin
	    if (ytran != LINEAR)
		if (ytran == LOG)
		    y = 10.0 ** y
		else
		    y = aelogr (y)
	}
end

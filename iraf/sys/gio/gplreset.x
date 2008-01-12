# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GPL_RESET -- Reset the state of the GPL common, forcing a call to
# re-initialize the cache in the next GADRAW call.  Should be called at
# GOPEN time and thereafter whenever the a WCS is modified or an polyline,
# polymarker, etc. attribute is set.

procedure gpl_reset()

bool	first_time
include	"gpl.com"
data	first_time /true/

begin
	if (first_time) {
	    op = 1
	    first_time = false
	} else
	    call gpl_flush()

	wcs = -1
	gp_out = NULL
	pl_type = POLYLINE
	last_point_inbounds = false
end

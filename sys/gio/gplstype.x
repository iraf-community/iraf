# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GPL_SETTYPE -- Set type (polyline, polymarker, fillarea) of point array PL.
# Determines instruction type generated when PL is flushed.

procedure gpl_settype (gp, type)

pointer	gp			# graphics descriptor
int	type			# type of instruction
include	"gpl.com"

begin
	if (op > 1 && pl_type != type)
	    call gpl_flush()

	if (type == POINTMODE) {
	    pl_type = POLYMARKER
	    pl_pointmode = YES
	} else {
	    pl_type = type
	    pl_pointmode = NO
	}
end

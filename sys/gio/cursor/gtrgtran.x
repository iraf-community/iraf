# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_GTRAN -- Get the workstation transformation.

procedure gtr_gtran (fd, x1, x2, y1, y2)

int	fd			# graphics stream to be set
real	x1, x2			# range of workstation viewport in X
real	y1, y2			# range of workstation viewport in Y
include	"gtr.com"

begin
	if (wstranset == YES) {
	    x1 = vx1
	    x2 = vx2
	    y1 = vy1
	    y2 = vy2
	} else {
	    x1 = 0
	    x2 = 1.0
	    y1 = 0
	    y2 = 1.0
	}
end

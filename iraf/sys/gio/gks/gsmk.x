# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSMK -- Set marker type.  Integer variable "marker" is set and
# stored in "gks.com".  Procedure gpm uses this value.

procedure gsmk (mtype)

int	mtype			# Marker type for polymarker
include	"gks.com"

begin
	switch (mtype) {
	case GPOINT:
	    gk_marker = GM_POINT
	case GPLUS:
	    gk_marker = GM_PLUS
	case GAST:
	    gk_marker = GM_BOX
	case GOMARK:
	    gk_marker = GM_DIAMOND
	case GXMARK:
	    gk_marker = GM_CROSS
	default:
	    gk_marker = GM_POINT
	}
end

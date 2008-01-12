# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQMK -- Query marker type.  Integer variable "marker" is read from
# "gks.com" and returned.  

procedure gqmk (ierr, mtype)

int	ierr			# Error indicator - no way it can be set
int	mtype			# Marker type for polymarker
include	"gks.com"

begin
	ierr = 0
	switch (gk_marker) {
	case GM_POINT:
	    mtype = GPOINT
	case GM_PLUS:
	    mtype = GPLUS
	case GM_BOX:
	    mtype = GAST
	case GM_DIAMOND:
	    mtype = GOMARK
	case GM_CROSS:
	    mtype = GXMARK
	default:
	    mtype = GPOINT
	}
end

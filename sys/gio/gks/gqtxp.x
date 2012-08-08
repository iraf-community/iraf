# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQTXP -- Inquire text path.

procedure gqtxp (ierror, path)

int	ierror			# Error indicator
int	path			# Text path - returned value.

int	text_path
int	gstati()
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    ierror = 7
	    path = -1
	    return
	} else
	    ierror = 0

	iferr {
	    text_path =  gstati (gp[gk_std], G_TXPATH)

	    switch (text_path) {
	    case (GT_LEFT):
	        path = GLEFT
	    case (GT_RIGHT):
	        path = GRIGHT
	    case (GT_UP):
	        path = GUP
	    case (GT_DOWN):
	        path = GDOWN
	    default:
	        path = GRIGHT
	    }
	} then {
	    ierror = 1
	    path = -1
	}
end

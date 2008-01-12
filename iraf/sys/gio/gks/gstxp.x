# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSTXP -- Set text path.

procedure gstxp (txp)

int	txp		# Text path to be set

begin
	switch (txp) {
	case GRIGHT:
	    call gsawi (G_TXPATH, GT_RIGHT)
	case GLEFT:
	    call gsawi (G_TXPATH, GT_LEFT)
	case GUP:
	    call gsawi (G_TXPATH, GT_UP)
	case GDOWN:
	    call gsawi (G_TXPATH, GT_DOWN)
	default:
	    call gsawi (G_TXPATH, GT_RIGHT)
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSTXAL -- Set format alignment.  

procedure gstxal (txalh, txalv)

int	txalh		# Horizontal alignment
int	txalv		# Vertical alignment

begin
	switch (txalh) {
	case GAHNOR:
	    call gsawi (G_TXHJUSTIFY, GT_NORMAL)
	case GALEFT:
	    call gsawi (G_TXHJUSTIFY, GT_LEFT)
	case GACENT:
	    call gsawi (G_TXHJUSTIFY, GT_CENTER)
	case GARITE:
	    call gsawi (G_TXHJUSTIFY, GT_RIGHT)
	default:
	    call gsawi (G_TXHJUSTIFY, GT_NORMAL)
	}

	switch (txalv) {
	case GAVNOR:
	    call gsawi (G_TXVJUSTIFY, GT_NORMAL)
	case GATOP:
	    call gsawi (G_TXVJUSTIFY, GT_TOP)
	case GACAP:
	    call gsawi (G_TXVJUSTIFY, GT_TOP)
	case GAHALF:
	    call gsawi (G_TXVJUSTIFY, GT_CENTER)
	case GABASE:
	    call gsawi (G_TXVJUSTIFY, GT_BOTTOM)
	case GABOTT:
	    call gsawi (G_TXVJUSTIFY, GT_BOTTOM)
	default:
	    call gsawi (G_TXVJUSTIFY, GT_NORMAL)
	}
end

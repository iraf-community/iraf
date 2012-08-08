# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQTXAL -- Inquire text alignment.

procedure gqtxal (ierror, txalh, txalv)

int	ierror			# Error indicator; ierror = 0 means no error
int	txalh			# Horizontal text alignment
int	txalv			# Vertical text alignment

int	justify
int	gstati()
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    ierror = 7
	    txalh = -1
	    txalv = -1
	    return
	} else
	    ierror = 0

	iferr {
	    # Get value of horizontal text justification
	    justify = gstati (gp[gk_std], G_TXHJUSTIFY)

	    switch (justify) {
	    case GT_NORMAL:
	        txalh = GAHNOR
	    case GT_CENTER:
	        txalh = GACENT
	    case GT_LEFT:
	        txalh = GALEFT
	    case GT_RIGHT:
	        txalh = GARITE
	    default:
	        txalh = GAHNOR
	    }

	    # Get value of vertical text justification
	    justify =  gstati (gp[gk_std], G_TXVJUSTIFY)

	    switch (justify) {
	    case GT_NORMAL:
		txalv = GAVNOR
	    case GT_CENTER:
		txalv = GAHALF
	    case GT_TOP:
		txalv = GATOP
	    case GT_BOTTOM:
		txalv = GABOTT
	    default:
		txalv = GAVNOR
	    }
	} then {
	    ierror = 1
	    txalv = -1
	    txalh = -1
	}
end

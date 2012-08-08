# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQTXCI -- Inquire text color index.

procedure gqtxci (ierror, coli)

int	ierror			# Error indicator
int	coli			# Color index - returned value.
int	gstati ()
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    ierror = 7
	    coli = -1
	    return
	} else
	    ierror = 0

	iferr {
	    coli = gstati (gp[gk_std], G_TXCOLOR)
	} then {
	    ierror = 1
	    coli = -1
	}
end

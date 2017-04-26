# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQCHH - Inquire character height.

procedure gqchh (ierror, chh)

int	ierror			# Error indicator
real	chh			# Character height, in world coordinates

real	dx, dy
real	gstatr()
include	"gks.com"
errchk	gstatr, ggscale

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    ierror = 7
	    chh = -1.0
	    return
	} else 
	    ierror = 0

	iferr {
	    chh = gstatr (gp[gk_std], G_CHARSIZE)

	    # The character height is expressed in NDC units.  It must be 
	    # converted to world coordinates before returning.

	    call ggscale (gp[gk_std], 0., 0., dx, dy)
	    chh = chh * dy
	} then {
	    ierror = 1
	    chh = -1.0
	}
end

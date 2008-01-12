# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSCHH -- Set character height.

procedure gschh (chh)

real	chh		# Character height in world coordinates

real	dx, dy, ndc_chh
include	"gks.com"

begin
	# Input chh is in world coordinates; it must be transformed to NDC.
	# Assuming spatial transformation is linear, input coordinates to
	# ggscale are not used and so are set to 0.0.

	call ggscale (gp[gk_std], 0.0, 0.0, dx, dy)
	if (dy != 0) {
	    ndc_chh = chh / dy
	    call gsawr (G_CHARSIZE, ndc_chh)
	} else
	    call gsawr (G_CHARSIZE, chh)
end

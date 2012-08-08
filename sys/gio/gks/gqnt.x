# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQNT -- Inquire normalization transformation (window and vport).  Note
# that this procedure gets the information for WCS ntnr, then resets to
# the current WCS before returning.

procedure gqnt (ntnr, errind, window, vport)

int	ntnr			# Normalization transformation number to query
int	errind			# Error indicator; errind = 0 means no error
real	window[4]		# Window coordinates for WCS ntnr
real	vport[4]		# Viewport coordinates for WCS ntnr

int	current_wcs
int	gstati()
include	"gks.com"
errchk	gstati, gseti, ggwind, ggview

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    errind = 7
	    window[1] = 0.0
	    window[2] = 0.0
	    window[3] = 0.0
	    window[4] = 0.0
	    vport[1] = -1.0
	    vport[2] = -1.0
	    vport[3] = -1.0
	    vport[4] = -1.0
	    return
	} else
	    errind = 0

	if (ntnr < 0 || ntnr > MAX_WCS) {
	    errind = 50
	    window[1] = 0.0
	    window[2] = 0.0
	    window[3] = 0.0
	    window[4] = 0.0
	    vport[1] = -1.0
	    vport[2] = -1.0
	    vport[3] = -1.0
	    vport[4] = -1.0
	    return
	}

	iferr {
	    current_wcs = gstati (gp[gk_std], G_WCS)

	    call gseti  (gp[gk_std], G_WCS, ntnr)
	    call ggwind (gp[gk_std], window[1], window[2], window[3], window[4])
	    call ggview (gp[gk_std], vport[1], vport[2], vport[3], vport[4])

	    call gseti  (gp[gk_std], G_WCS, current_wcs)
	} then {
	    errind = 1
	    window[1] = 0.0
	    window[2] = 0.0
	    window[3] = 0.0
	    window[4] = 0.0
	    vport[1] = -1.0
	    vport[2] = -1.0
	    vport[3] = -1.0
	    vport[4] = -1.0
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQCLIP -- Inquire value of clipping flag

procedure gqclip (errind, iclip, iar)

int	errind			# Error indicator
int	iclip			# Clipping flag - returned value
real	iar[4]			# Clipping array 

int	gstati()
include	"gks.com"

begin
	# Until I know what this argument is, set iar to full viewport.  
	# Consulting with NCAR was not enlightning.  This argument (iar) 
	# is not documented in the GKS level 0A standard.
	iar[1] = 0.0
	iar[2] = 1.0
	iar[3] = 0.0
	iar[4] = 1.0

	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    errind = 7
	    iclip = -1
	    return
	} else
	    errind = 0

	iferr {
	    iclip = gstati (gp[gk_std], G_CLIP)
	} then {
	    errind = 1
	    iclip = -1
	}
end

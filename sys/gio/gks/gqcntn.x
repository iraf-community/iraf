# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQCNTN -- Inquire current normalization transformation number (WCS).

procedure gqcntn (errind, cntr)

int	errind		# Error indicator; errind = 0 means no error
int	cntr		# Current normalization transformation  number
int	gstati()
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    errind = 7
	    cntr = -1
	    return
	} else
	    errind = 0

	iferr {
	    cntr = gstati (gp[gk_std], G_WCS)
	} then {
	    errind = 1
	    cntr = -1
	}
end

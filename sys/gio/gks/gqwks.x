# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GQWKS -- Inquire workstation state.  State is either ACTIVE or INACTIVE;
# this information has been stored in gks.com by GACWK.

procedure gqwks (wkid, errind, state)

int	wkid			# Workstation id for inquire
int	errind			# Error indicator
int	state			# Returned state value: ACTIVE or INACTIVE
include	"gks.com"

begin
	errind = 0
	if (wkid > NDEV)
	    errind = 1
	else
	    state = gk_status[wkid]
end

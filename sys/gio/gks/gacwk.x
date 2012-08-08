# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GACWK -- Activate workstation.

procedure gacwk (wkid)

int	wkid 		# Workstation identifier
include	"gks.com"

begin
	# This procedure sets the active flag for a particular workstation.
	gk_status[wkid] = ACTIVE

	# Also, set gk_std to be the first activated workstation.  Once
	# gk_std has been set, it will no longer = NULL.
	if (gk_std == NULL)
	    gk_std = wkid
end

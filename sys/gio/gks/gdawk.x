# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GDAWK -- Deactivate workstation.

procedure gdawk (wkid)

int	wkid		# Workstation identifier
int	i
include	"gks.com"

begin
	# This procedure sets the status flag to INACTIVE for a particular
	# device.  Because this workstation may have been the reference
	# workstation, gk_std, it may also necessary to update gk_std.
	# In this case, the reference workstation will be the one with the
	# lowest workstation id number.

	gk_status[wkid] = INACTIVE

	if (wkid == gk_std) {
	    gk_std = NULL
	    # Find next activated workstation, if any
	    do i = 1, NDEV {
	        if (gk_status[i] == ACTIVE) {
		    gk_std = i
		    break
		}
	    }
	}
end

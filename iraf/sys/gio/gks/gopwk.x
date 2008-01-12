# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GOPWK -- Open workstation.

procedure gopwk (wkid, conid, wtype)

int	wkid		# Workstation identifier
int	conid		# Connection identifier, not used.
int	wtype		# Workstation type 

include	"gks.com"


begin
	# This procedure sets "gp[wkid]" to be the "gp" of workstation "wkid".
	# Procedure gopen has been called by the calling routine.  The wkid
	# runs sequentially from 1 to the maximum allowable number of open
	# workstations.  Parameter wtype is the gp returned from gopen.

	gp[wkid] = wtype
end

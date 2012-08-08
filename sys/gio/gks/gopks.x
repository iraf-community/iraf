# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GOPKS -- Open GKS.  In the GIO implementation, this routine sets the
# file to receive error output to STDERR and initializes all possible
# workstations to inactive.  It also initializes the ASF array to GINDIV.

procedure gopks (errfil)

int	errfil		# Unit number for error output
int	i
include	"gks.com"

begin
	# This procedure initializes the gk_status and gk_std variables.
	do i = 1, NDEV 
	    gk_status[i] = INACTIVE
	
	gk_std = NULL

	do i = 1, NASF
	    gk_asf[i] = GINDIV
end

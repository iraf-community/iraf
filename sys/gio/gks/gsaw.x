# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSAW[IR] -- Sets integer or real parameters for all active workstations.

procedure gsawi (param, value)

int	param		# Parameter to be set
int	value		# New value for parameter

int	i
include	"gks.com"

begin
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gseti (gp[i], param, value)
	}
end


procedure gsawr (param, value)

int	param		# Parameter to be set
real	value		# New value for parameter

int	i
include	"gks.com"

begin
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gsetr (gp[i], param, value)
	}
end

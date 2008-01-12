# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"ccp.h"

# CCP_PMSET -- Set the polymarker attributes.

procedure ccp_pmset (gki)

short	gki[ARB]		# attribute structure
pointer	pm
include	"ccp.com"

begin
	pm = CCP_PMAP(g_cc)
	PM_LTYPE(pm) = gki[GKI_PMSET_MT]
	PM_WIDTH(pm) = gki[GKI_PMSET_MW]
	PM_COLOR(pm) = gki[GKI_PMSET_CI]
end

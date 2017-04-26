# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

# STG_PMSET -- Set the polymarker attributes.

procedure stg_pmset (gki)

short	gki[ARB]		# attribute structure
pointer	pm
include	"stdgraph.com"

begin
	pm = SG_PMAP(g_sg)
	PM_LTYPE(pm) = gki[GKI_PMSET_MT]
	PM_WIDTH(pm) = max (1, nint (GKI_UNPACKREAL (gki[GKI_PMSET_MW])))
	PM_COLOR(pm) = gki[GKI_PMSET_CI]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"gkt.h"

# GKT_PMSET -- Set the polymarker attributes.

procedure gkt_pmset (gki)

short	gki[ARB]		# attribute structure
pointer	pm
include	"gkt.com"

begin
	pm = GKT_PMAP(g_kt)
	PM_LTYPE(pm) = gki[GKI_PMSET_MT]
	PM_WIDTH(pm) = gki[GKI_PMSET_MW]
	PM_COLOR(pm) = gki[GKI_PMSET_CI]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"../lib/ids.h"

# IDS_PMSET -- Set the polymarker attributes.

procedure ids_pmset (gki)

short	gki[ARB]		# attribute structure
pointer	pm
include	"../lib/ids.com"

begin
	pm = IDS_PMAP(i_kt)
	PM_LTYPE(pm) = gki[GKI_PMSET_MT]
	PM_WIDTH(pm) = gki[GKI_PMSET_MW]
	PM_COLOR(pm) = gki[GKI_PMSET_CI]
end

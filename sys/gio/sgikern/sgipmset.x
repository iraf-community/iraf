# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"sgi.h"

# SGI_PMSET -- Set the polymarker attributes.

procedure sgi_pmset (gki)

short	gki[ARB]		# attribute structure
pointer	pm
include	"sgi.com"

begin
	pm = SGI_PMAP(g_kt)
	PM_LTYPE(pm) = gki[GKI_PMSET_MT]
	PM_WIDTH(pm) = gki[GKI_PMSET_MW]
	PM_COLOR(pm) = gki[GKI_PMSET_CI]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

# STG_FASET -- Set the fillarea attributes.

procedure stg_faset (gki)

short	gki[ARB]		# attribute structure
pointer	fa
include	"stdgraph.com"

begin
	fa = SG_FAAP(g_sg)
	FA_STYLE(fa) = gki[GKI_FASET_FS]
	FA_COLOR(fa) = gki[GKI_FASET_CI]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"gkt.h"

# GKT_FASET -- Set the fillarea attributes.

procedure gkt_faset (gki)

short	gki[ARB]		# attribute structure
pointer	fa
include	"gkt.com"

begin
	fa = GKT_FAAP(g_kt)
	FA_STYLE(fa) = gki[GKI_FASET_FS]
	FA_COLOR(fa) = gki[GKI_FASET_CI]
end

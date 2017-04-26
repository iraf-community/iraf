# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"ccp.h"

# CCP_FASET -- Set the fillarea attributes.

procedure ccp_faset (gki)

short	gki[ARB]		# attribute structure
pointer	fa
include	"ccp.com"

begin
	fa = CCP_FAAP(g_cc)
	FA_STYLE(fa) = gki[GKI_FASET_FS]
	FA_COLOR(fa) = gki[GKI_FASET_CI]
end

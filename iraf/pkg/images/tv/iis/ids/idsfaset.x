# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"../lib/ids.h"

# IDS_FASET -- Set the fillarea attributes.

procedure ids_faset (gki)

short	gki[ARB]		# attribute structure
pointer	fa
include	"../lib/ids.com"

begin
	fa = IDS_FAAP(i_kt)
	FA_STYLE(fa) = gki[GKI_FASET_FS]
	FA_COLOR(fa) = gki[GKI_FASET_CI]
end

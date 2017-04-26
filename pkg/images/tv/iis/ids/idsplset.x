# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"../lib/ids.h"

# IDS_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure ids_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl

include	"../lib/ids.com"

begin
	pl = IDS_PLAP(i_kt)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = gki[GKI_PLSET_LW]
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

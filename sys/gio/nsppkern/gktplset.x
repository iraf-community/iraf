# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"gkt.h"

# GKT_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure gkt_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl
include	"gkt.com"

begin
	pl = GKT_PLAP(g_kt)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = gki[GKI_PLSET_LW]
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

# STG_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure stg_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl
include	"stdgraph.com"

begin
	pl = SG_PLAP(g_sg)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = max (1, nint (GKI_UNPACKREAL (gki[GKI_PLSET_LW])))
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

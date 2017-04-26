# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"ccp.h"

# CCP_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure ccp_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl
include	"ccp.com"

begin
	pl = CCP_PLAP(g_cc)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = gki[GKI_PLSET_LW]
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

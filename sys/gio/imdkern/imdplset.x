# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"imd.h"

# IMD_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure imd_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl
include	"imd.com"

begin
	pl = IMD_PLAP(g_kt)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = gki[GKI_PLSET_LW]
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

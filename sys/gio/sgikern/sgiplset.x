# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"sgi.h"

# SGI_PLSET -- Set the polyline attributes.  The polyline width parameter is
# passed to the encoder as a packed floating point number, i.e., int(LWx100).

procedure sgi_plset (gki)

short	gki[ARB]		# attribute structure
pointer	pl
include	"sgi.com"

begin
	pl = SGI_PLAP(g_kt)
	PL_LTYPE(pl) = gki[GKI_PLSET_LT]
	PL_WIDTH(pl) = gki[GKI_PLSET_LW]
	PL_COLOR(pl) = gki[GKI_PLSET_CI]
end

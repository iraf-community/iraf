# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gset.h>
include	"gkt.h"

# GKT_FONT -- Set the character font.  The roman font is normal.  Bold is
# implemented by increasing the vector line width; care must be taken to
# set GKT_WIDTH so that the other vector drawing procedures remember to
# change the width back.  The italic font is implemented in the character
# generator by a geometric transformation.

procedure gkt_font (font)

int	font			# code for font to be set
int	pk1, pk2, width
include	"gkt.com"

begin
	pk1 = GKI_PACKREAL(1.0)
	pk2 = GKI_PACKREAL(2.0)

	width = GKT_WIDTH(g_kt)

	if (font == GT_BOLD) {
	    if (width != pk2) {
		call optn (*"inten", *"high")
		width = pk2
	    }
	} else {
	    if (GKI_UNPACKREAL(width) > 1.5) {
		call optn (*"inten", *"low")
		width = pk1
	    }
	}

	GKT_WIDTH(g_kt) = width
end

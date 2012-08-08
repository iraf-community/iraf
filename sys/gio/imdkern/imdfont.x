# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gset.h>
include	"imd.h"

# IMD_FONT -- Set the character font.  The roman font is normal.  Bold is
# implemented by increasing the vector line width; care must be taken to
# set IMD_WIDTH so that the other vector drawing procedures remember to
# change the width back.  The italic font is implemented in the character
# generator by a geometric transformation.

procedure imd_font (font)

int	font			# code for font to be set
int	pk2, width
include	"imd.com"

begin
	width = IMD_WIDTH(g_kt)
	pk2   = GKI_PACKREAL(2.0)

	if (font == GT_BOLD) {
	    if (width != pk2) {
		call idk_linewidth (g_out, 2)
		width = pk2
	    }
	} else
	    call idk_linewidth (g_out, nint (GKI_UNPACKREAL(width)))

	IMD_WIDTH(g_kt) = width
end

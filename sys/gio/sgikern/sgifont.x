# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gset.h>
include	"sgi.h"

# SGI_FONT -- Set the character font.  The roman font is normal.  Bold is
# implemented by increasing the vector line width; care must be taken to
# set SGI_WIDTH so that the other vector drawing procedures remember to
# change the width back.  The italic font is implemented in the character
# generator by a geometric transformation.

procedure sgi_font (font)

int	font			# code for font to be set

int	normal, bold
int	pk1, pk2, width
include	"sgi.com"

begin
	width = SGI_WIDTH(g_kt)
	normal = 0
	bold = 1

	pk1 = GKI_PACKREAL(real(normal))
	pk2 = GKI_PACKREAL(real(bold))

	if (font == GT_BOLD) {
	    if (width != pk2) {
		call sgk_linewidth (g_out, bold)
		width = pk2
	    }
	} else {
	    if (width != pk1) {
	        call sgk_linewidth (g_out, normal)
		width = pk1
	    }
	}

	SGI_WIDTH(g_kt) = width
end

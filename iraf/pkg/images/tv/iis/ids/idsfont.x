# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gset.h>
include	"../lib/ids.h"

# IDS_FONT -- Set the character font.  The roman font is normal.  Bold is
# implemented by increasing the vector line width; care must be taken to
# set IDS_WIDTH so that the other vector drawing procedures remember to
# change the width back.  The italic font is implemented in the character
# generator by a geometric transformation.

procedure ids_font (font)

int	font			# code for font to be set
int	pk1, pk2, width
include	"../lib/ids.com"

begin
	pk1 = GKI_PACKREAL(1.0)
	pk2 = GKI_PACKREAL(2.0)

	width = IDS_WIDTH(i_kt)

	if (font == GT_BOLD) {
	    if (width != pk2) {
		# Name collision with ids_open !!
		# call ids_optn (*"inten", *"high")
		width = pk2
	    }
	} else {
	    if (GKI_UNPACKREAL(width) > 1.5) {
		# Name collision with ids_open !!
		# call ids_optn (*"inten", *"low")
		width = pk1
	    }
	}

	IDS_WIDTH(i_kt) = width
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<gki.h>
include	<gset.h>
include	"sgi.h"
include	"font.h"

define	ITALIC_TILT	0.30	# fraction of xsize to tilt italics at top


# SGI_DRAWCHAR -- Draw a character of the given size and orientation at the
# given position.

int procedure sgi_drawchar (ch, x, y, xsize, ysize, orien, font)

char	ch			# character to be drawn
int	x, y			# lower left GKI coords of character
int	xsize, ysize		# width, height of char in GKI units
int	orien			# orientation of character (0 degrees normal)
int	font			# desired character font

int	mx, my
real	px, py, coso, sino, theta
int	stroke, tab1, tab2, i, pen, width
int	bitupk()
include	"font.com"
include	"greek.com"
include	"sgi.com"

begin
	if (ch < CHARACTER_START || ch > CHARACTER_END)
	    i = '?' - CHARACTER_START + 1
	else
	    i = ch  - CHARACTER_START + 1

	# Set the font.
	call sgi_font (font)

	if (font == GT_GREEK) {
	    width = gchwid[i]
	    tab1 = gchidx[i]
	    tab2 = gchidx[i+1] - 1
	} else {
	    width = chrwid[i]
	    tab1 = chridx[i]
	    tab2 = chridx[i+1] - 1
	}

	theta = -DEGTORAD(orien)
	coso = cos(theta)
	sino = sin(theta)

	do i = tab1, tab2 {
	    if (font == GT_GREEK)
	        stroke = gchtab[i]
	    else
	        stroke = chrtab[i]

	    px  = bitupk (stroke, COORD_X_START,   COORD_X_LEN)
	    py  = bitupk (stroke, COORD_Y_START,   COORD_Y_LEN)
	    pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN)

	    # Scale size of character.
	    px = px / width  * xsize
	    py = py / FONT_HEIGHT * ysize

	    # The italic font is implemented applying a tilt.
	    if (font == GT_ITALIC)
		px = px + ((py / ysize) * xsize * ITALIC_TILT)

	    # Rotate and shift.
	    mx = x + px * coso + py * sino
	    my = y - px * sino + py * coso

	    # Draw the line segment or move pen.
	    if (pen == 0)
		call sgk_move (g_out, mx, my)
	    else
		call sgk_draw (g_out, mx, my)
	}

	return (int(real(width) / real(FONT_WIDTH) * xsize))
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<gset.h>
include	<gki.h>
include	"stdgraph.h"
include	"font.h"

define	ITALIC_TILT	0.30	# fraction of xsize to tilt italics at top
define	MAXPTS		40	# max points in a char drawing polyline


# STG_DRAWCHAR -- Draw a character of the given size and orientation at the
# given position.

procedure stg_drawchar (ch, x, y, xsize, ysize, orien, font)

char	ch			# character to be drawn
int	x, y			# lower left GKI coords of character
int	xsize, ysize		# width, height of char in GKI units
int	orien			# orientation of character (0 degrees normal)
int	font			# desired character font

pointer	pl, tx
real	px, py, coso, sino, theta
int	stroke, tab1, tab2, i, pen, mx, my
int	save_ltype, save_lwidth, save_color
int	bitupk()
include	"font.com"
include	"stdgraph.com"

begin
	if (ch < CHARACTER_START || ch > CHARACTER_END)
	    i = '?' - CHARACTER_START + 1
	else
	    i = ch  - CHARACTER_START + 1

	# Set the font.
	if (SG_TXFONT(g_sg) != font) {
	    call stg_ctrl1 ("TF", font - GT_ROMAN + 1)
	    SG_TXFONT(g_sg) = font
	}

	# Since we will be using the polyline generator, set the polyline
	# linetype to solid and save the current linetype for later restoration.

	pl = SG_PLAP(g_sg)
	tx = SG_TXAP(g_sg)
	save_color = PL_COLOR(pl)
	save_ltype = PL_LTYPE(pl)
	save_lwidth = PL_WIDTH(pl)
	PL_COLOR(pl) = TX_COLOR(tx)
	PL_LTYPE(pl) = GL_SOLID
	PL_WIDTH(pl) = 1

	tab1 = chridx[i]
	tab2 = chridx[i+1] - 1

	theta = -DEGTORAD(orien)
	coso = cos(theta)
	sino = sin(theta)

	do i = tab1, tab2 {
	    stroke = chrtab[i]
	    px  = bitupk (stroke, COORD_X_START,   COORD_X_LEN)
	    py  = bitupk (stroke, COORD_Y_START,   COORD_Y_LEN)
	    pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN)

	    # Scale size of character.
	    px = px / FONT_WIDTH  * xsize
	    py = py / FONT_HEIGHT * ysize

	    # The italic font is implemented applying a tilt.
	    if (font == GT_ITALIC)
		px = px + ((py / ysize) * xsize * ITALIC_TILT)

	    # Rotate and shift.
	    mx = x + px * coso + py * sino
	    my = y - px * sino + py * coso

	    # Draw the line segment or move pen.
	    if (pen == 0)
		call sgch_move (mx, my)
	    else
		call sgch_draw (mx, my)
	}

	# Flush any remaining points.
	call sgch_flush()

	# Restore polyline linetype and color.
	PL_LTYPE(pl) = save_ltype
	PL_WIDTH(pl) = save_lwidth
	PL_COLOR(pl) = save_color
end


# SGCH_MOVE -- Start accumulating a new polyline.

procedure sgch_move (mx, my)

int	mx, my
short	pl[MAXPTS], op
common	/sgchcm/ pl, op

begin
	call sgch_flush()

	pl[1] = mx
	pl[2] = my
	op = 3
end


# SGCH_DRAW -- Add a point to the polyline.

procedure sgch_draw (mx, my)

int	mx, my
short	pl[MAXPTS], op
common	/sgchcm/ pl, op

begin
	pl[op] = mx
	pl[op+1] = my
	op = min (MAXPTS, op + 2)
end


# SGCH_FLUSH -- Flush the polyline to the output device.

procedure sgch_flush()

int	npts
short	pl[MAXPTS], op
common	/sgchcm/ pl, op

begin
	if (op > 2) {
	    npts = op / 2
	    call stg_polyline (pl, npts)
	}
	op = 1
end

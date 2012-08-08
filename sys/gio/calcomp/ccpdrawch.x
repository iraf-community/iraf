# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<gki.h>
include	<gset.h>
include	"ccp.h"
include	"font.h"

define	ITALIC_TILT	0.30	# fraction of xsize to tilt italics at top
define	MAX_STROKESIZE	32	# max number of vectors making up one stroke
define	CALCOMP_CHSTART	16	# maximum calcomp special symbol plus 1
define	SYMBOL_ASPECT	1.17	# calcomp height = 7/6 width for normal spacing.
define	LOW_REDRAWS	5	# multiple traces for low-quality bold text
define	HIGH_REDRAWS	9	# multiple traces for high-quality bold text


# CCP_DRAWCHAR -- Draw a character of the given size and orientation at the
# given position.

procedure ccp_drawchar (ch, x, y, xsize, ysize, orien, font, quality)

char	ch			# character to be drawn
int	x, y			# lower left GKI coords of character
int	xsize, ysize		# char width and height in unscaled GKI units
int	orien			# orientation of character (0 degrees normal)
int	font			# desired character font
int	quality			# quality control -- low(calcomp); other(iraf)

real	px, py, coso, sino, theta, xto_nicesize, yto_nicesize
real	sx[MAX_STROKESIZE], sy[MAX_STROKESIZE]
int	stroke, tab1, tab2, i, j, pen
int	bitupk()

include "ccp.com"
include	"font.com"

begin
	# Compute correction factor for absolute physical character size.
	# This also corrects for distortion of high-qual text if xscale<>yscale.
	xto_nicesize = g_xdefault_scale / g_xndcto_p
	yto_nicesize = g_ydefault_scale / g_yndcto_p

	# Set the font.
	call ccp_font (font)

	if (quality == GT_LOW) {
	    # If low text quality requested, draw with Calcomp's SYMBOL call.
	    # We avoid machine-dependency word-size problems by always
	    # calling SYMBOL only from here, one char per call.
	    # Calcomp's SYMBOL expects height as only size; aspect is height
	    # = 7/6 (width) for normal character spacing.

	    call ccx_intersymbol (XTRAN(x),YTRAN(y), real(xsize) * xto_nicesize,
	        real(ysize) * yto_nicesize, ch, real(orien))

	} else {
	    # Text quality requested is not low; draw font either with single-
	    # width line or bold, via ccp_drawseg.

	    if (ch < CHARACTER_START || ch > CHARACTER_END)
		i = '?' - CHARACTER_START + 1
	    else
		i = ch  - CHARACTER_START + 1

	    tab1 = chridx[i]
	    tab2 = chridx[i+1] - 1

	    if (tab2 - tab1 + 1 > MAX_STROKESIZE) {
		call eprintf (
		    "CCP KERNEL WARNING: up-dimension MAX_STROKESIZE\n")
		call eprintf (
		    "in module ccp_drawch; new stroke size %d, char %s\n")
		    call pargi (tab2 - tab1 + 1)
		    call pargc (ch)
		tab2 = tab1 + MAX_STROKESIZE - 1
	    }

	    theta = -DEGTORAD(orien)
	    coso = cos(theta)
	    sino = sin(theta)

	    j = 0
	    do i = tab1, tab2 {
		stroke = chrtab[i]
		px  = bitupk (stroke, COORD_X_START,   COORD_X_LEN)
		py  = bitupk (stroke, COORD_Y_START,   COORD_Y_LEN)
		pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN)

		# Scale size of character in unwarped (xscale == yscale) system.
		px = px / FONT_WIDTH  * xsize
		py = py / FONT_HEIGHT * ysize

		# The italic font is implemented applying a tilt.
		if (font == GT_ITALIC)
		    px = px + ((py / ysize) * xsize * ITALIC_TILT)

		if (pen == 0 && j > 0) {	# new stroke segment; draw last
		    if (j > 1) 
			call ccx_interpoly (sx, sy, j, quality)
		    j = 0
		} 

		# Rotate, shift (unwarped), then correct for xscale <> yscale.
		j     = j + 1
		sx[j] = XTRAN(x + ( px * coso + py * sino) * xto_nicesize)
		sy[j] = YTRAN(y + (-px * sino + py * coso) * yto_nicesize)
	    }

	    # last stroke segment:
	    if (j > 1)
	        call ccx_interpoly (sx, sy, j, quality)
	}
end


# CCX_INTERPOLY -- intermediate routine to 1) pass simple draw instruction to 
# calcomp plot routines if linewidth single or bold method = penchange, or 
# 2) simulate bold text by offsetting to the four corners and four edges
# of a box surrounding the character.

procedure ccx_interpoly (x, y, npts, quality)

real	x[ARB],y[ARB] 	# plotter-unit coordinates to be drawn as polyline
int	npts		# number points in x,y
int	quality		# text quality (distinguish between medium and high)

int	i, j, num_redraws, twidth
real	xp, yp
real	xoff[HIGH_REDRAWS],yoff[HIGH_REDRAWS]

include	"ccp.com"

data	xoff/0., 1., 0., -1.,  0.,    1., -1., -1.,  1./
data	yoff/0., 0., 1.,  0., -1.,    1.,  1., -1., -1./

begin
	if (npts <= 0) 
	    return

	# If line width override is on, or linewidth is single, do simple move
	# and draws.

	num_redraws = 1
	twidth = nint(GKI_UNPACKREAL(CCP_WIDTH(g_cc)))
	if (!g_lwover)
	    if (g_lwtype == 'p' && twidth >= 1)
		call ccp_lwidth (twidth)
	    else if (twidth > 1 && quality == GT_HIGH)
		num_redraws = HIGH_REDRAWS
	    else
		num_redraws = LOW_REDRAWS

	if (num_redraws == 1) {
	    call plot (x[1], y[1], CCP_UP)
	    g_max_x = max (x[1], g_max_x)

	    if (npts == 1) {	# single pt is special case; drop pen
		call plot (x[1], y[1], CCP_DOWN)
	    } else {		# draw normally
		do i = 2, npts {
		    call plot (x[i], y[i], CCP_DOWN)
		    g_max_x = max (x[i], g_max_x)
		}
	    }
	} else {
	    do i = 1, num_redraws {
		xp = x[1] + xoff[i] * g_plwsep
		yp = y[1] + yoff[i] * g_plwsep
		call plot (xp, yp, CCP_UP)
		g_max_x = max (xp, g_max_x)

		if (npts == 1) {	# single pt is special case; drop pen
		    call plot (xp, yp, CCP_DOWN)
		} else {		# draw normally
		    do j = 2, npts {
			xp = x[j] + xoff[i] * g_plwsep
			yp = y[j] + yoff[i] * g_plwsep
			call plot (xp, yp, CCP_DOWN)
			g_max_x = max (xp, g_max_x)
		    }
		}
	    }
	}
end


# CCX_INTERSYMBOL -- routine intermediate to Calcomp SYMBOL routine; handles
# bold text.

procedure ccx_intersymbol (x,y, xsize,ysize, ch, orien)

real	x,y		# plotter-unit coords of lower left of character
real	xsize,ysize	# char width, height in GKI units scaled to "nice" sizes
char	ch		# character to be drawn
real	orien		# degrees counterclockwise from +x axis to text path

int	i, nsym, symchar, num_redraws
real	xp,yp, xoff[HIGH_REDRAWS],yoff[HIGH_REDRAWS], csize

include	"ccp.com"

data	xoff/0., 1., 0., -1.,  0.,    1., -1., -1.,  1./
data	yoff/0., 0., 1.,  0., -1.,    1.,  1., -1., -1./

begin
	symchar = int (ch)
	nsym    = 1

	if (ch < CALCOMP_CHSTART && ch >= 0) {
	    nsym = -1
	} else if (ch < ' ' || ch > '~')
	    ch = '~'

	# Since we are only called if text_quality == low, implement
	# bold text with only the center and edge positions (LOW_REDRAWS).
	num_redraws = 1
	if (!g_lwover && nint(GKI_UNPACKREAL(CCP_WIDTH(g_cc))) > 1)
	    num_redraws = LOW_REDRAWS

	# Set the size as the height of the character in device units; we
	# start with the width to avoid overlapping, and we use the default
	# scale, which results in reasonable-sized characters; the specified
	# scale would produce strange results as orien passes from 0 to 90.

	csize = min (xsize * g_xndcto_p * SYMBOL_ASPECT, ysize * g_yndcto_p)

	do i = 1, num_redraws {
	    xp = x + xoff[i] * g_plwsep
	    yp = y + yoff[i] * g_plwsep
	    call symbol (xp, yp, csize, symchar, orien, nsym)
	    g_max_x = max (xp + csize, g_max_x)
	}
end

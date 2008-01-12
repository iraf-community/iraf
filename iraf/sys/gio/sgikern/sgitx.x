# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<gset.h>
include	<gki.h>
include	"sgi.h"
include	"font.h"

define	BASECS_X	12	# Base (size 1.0) char width in GKI coords.
define	BASECS_Y	12	# Base (size 1.0) char height in GKI coords.


# SGI_TEXT -- Draw a text string.  The string is drawn at the position (X,Y)
# using the text attributes set by the last GKI_TXSET instruction.  The text
# string to be drawn may contain embedded set font escape sequences of the
# form \fR (roman), \fG (greek), etc.  We break the input text sequence up
# into segments at font boundaries and draw these on the output device,
# setting the text size, color, font, and position at the beginning of each
# segment.

procedure sgi_text (xc, yc, text, n)

int	xc, yc			# where to draw text string
short	text[ARB]		# text string
int	n			# number of characters

real	x, y, dx, dy, tsz, offset, cosv, sinv
int	x1, x2, y1, y2, orien
int	x0, y0, gki_dx, gki_dy, ch, cw
int	xstart, ystart, newx, newy
int	totlen, polytext, font, seglen, totwidth
pointer	sp, seg, ip, op, tx, first
int	stx_segment(), sgi_drawchar()
include	"sgi.com"

real	g_dx, g_dy		# scale GKI to window coords
int	g_x1, g_y1		# origin of device window
int	g_x2, g_y2		# upper right corner of device window
data	g_dx /1.0/, g_dy /1.0/
data	g_x1 /0/, g_y1 /0/, g_x2 /GKI_MAXNDC/, g_y2 / GKI_MAXNDC/

begin
	call smark (sp)
	call salloc (seg, n + 2, TY_CHAR)

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Set pointer to the text attribute structure.
	tx = SGI_TXAP(g_kt)

	# Set the text size and color if not already set.  Both should be
	# invalidated when the screen is cleared.  Text color should be
	# invalidated whenever another color is set.  The text size was
	# set by sgi_txset, and is just a scaling factor.

	SGI_TXSIZE(g_kt) = TX_SIZE(tx)
	if (TX_COLOR(tx) != SGI_COLOR(g_kt)) {
	    call sgi_color (TX_COLOR(tx))
	    SGI_COLOR(g_kt) = TX_COLOR(tx)
	}

	# Set the linetype to a solid line.
	if (SGI_TYPE(g_kt) != GL_SOLID) {
	    call sgi_linetype (GL_SOLID)
	    SGI_TYPE(g_kt) = GL_SOLID
	}

	# No discreet character sizes, so just scale the base sizes.
	tsz = GKI_UNPACKREAL(TX_SIZE(tx))	# scale factor
	ch = SGI_CHARHEIGHT(g_kt,1) * tsz
	cw = SGI_CHARWIDTH(g_kt,1) * tsz

	# Break the text string into segments at font boundaries and count
	# the total number of printable characters.

	totlen = stx_segment (text, n, Memc[seg], TX_FONT(tx), cw, totwidth)

	# Compute the text drawing parameters, i.e., the coordinates of the
	# first character to be drawn, the step between successive characters,
	# and the polytext flag (GKI coords).

	call stx_parameters (xc,yc, totlen, totwidth, x0,y0, gki_dx,gki_dy,
	    polytext, orien)

	# Draw the segments, setting the font at the beginning of each segment.
	# The first segment is drawn at (X0,Y0).  The separation between
	# characters is DX,DY.  A segment is drawn as a block if the polytext
	# flag is set, otherwise each character is drawn individually.

	x  = x0 * g_dx + g_x1
	y  = y0 * g_dy + g_y1
	dx = gki_dx * g_dx
	dy = gki_dy * g_dy
	cosv = cos (-DEGTORAD(orien))
	sinv = sin (-DEGTORAD(orien))

	for (ip=seg;  Memc[ip] != EOS;  ip=ip+1) {
	    # Process the font control character heading the next segment.
	    font = Memc[ip]
	    ip = ip + 1
	    
	    # Draw the segment.
	    while (Memc[ip] != EOS) {
		# Clip leading out of bounds characters.
		for (;  Memc[ip] != EOS;  ip=ip+1) {
		    x1 = x; x2 = x1 + cw
		    y1 = y; y2 = y1 + ch

		    if (x1 >= g_x1 && x2 <= g_x2 && y1 >= g_y1 && y2 <= g_y2)
			break
		    else {
			x = x + dx
			y = y + dy
		    }

		    if (polytext == NO) {
			ip = ip + 1
			break
		    }
		}

		# Coords of first char to be drawn.
		xstart = x
		ystart = y

		# Move OP to first out of bounds char.
		for (op=ip;  Memc[op] != EOS;  op=op+1) {
		    x1 = x; x2 = x1 + cw
		    y1 = y; y2 = y1 + ch

		    if (x1 <= g_x1 || x2 >= g_x2 || y1 <= g_y1 || y2 >= g_y2)
			break
		    else {
			x = x + dx
			y = y + dy
		    }

		    if (polytext == NO) {
			op = op + 1
			break
		    }
		}

		# Count number of inbounds chars.
		seglen = op - ip

		# Leave OP pointing to the end of this segment.
		if (polytext == NO)
		    op = ip + 1
		else {
		    while (Memc[op] != EOS)
			op = op + 1
		}

		# Compute X,Y of next segment.
		newx = xstart + (dx * (op - ip))
		newy = ystart +  dy

		# Quit if no inbounds chars.
		if (seglen == 0) {
		    x = newx
		    y = newy
		    ip = op
		    next
		}

		# Output the inbounds chars.

		first = ip
		x = xstart
		y = ystart

		while (seglen > 0 && (polytext == YES || ip == first)) {
		    offset = sgi_drawchar (Memc[ip], nint(x), nint(y), cw, ch,
			orien, font)
		    ip = ip + 1
		    seglen = seglen - 1
		    x = x + (offset * cosv)
		    y = y - (offset * sinv)
		}

		x = newx
		y = newy
		ip = op
	    }
	}

	call sfree (sp)
end


# STX_SEGMENT -- Process the text string into segments, in the process
# converting from type short to char.  The only text attribute that can
# change within a string is the font, so segments are broken by \fI, \fG,
# etc. font select sequences embedded in the text.  The segments are encoded
# sequentially in the output string.  The first character of each segment is
# the font number.  A segment is delimited by EOS.  A font number of EOS
# marks the end of the segment list.  The output string is assumed to be
# large enough to hold the segmented text string.

int procedure stx_segment (text, n, out, start_font, cw, totwidth)

short	text[ARB]		# input text
int	n			# number of characters in text
char	out[ARB]		# output string
int	start_font		# initial font code
int	cw			# default character width
int	totwidth		# seg width in GKI units

int 	i
int	ip, op
int	totlen, font

include	"font.com"
include	"greek.com"

begin
	out[1] = start_font
	font = start_font
	totlen = 0
	totwidth = 0
	op = 2

	for (ip=1;  ip <= n;  ip=ip+1) {
	    if (text[ip] == '\\' && text[ip+1] == 'f') {
		# Select font.
		out[op] = EOS
		op = op + 1
		ip = ip + 2

		switch (text[ip]) {
		case 'B':
		    font = GT_BOLD
		case 'I':
		    font = GT_ITALIC
		case 'G':
		    font = GT_GREEK
		default:
		    font = GT_ROMAN
		}

		out[op] = font
		op = op + 1

	    } else {
		# Deposit character in segment.
                if (text[ip] < CHARACTER_START || text[ip] > CHARACTER_END)
                    i = '?' - CHARACTER_START + 1
                else
                    i = text[ip]  - CHARACTER_START + 1
 
		if (font == GT_GREEK) {
		    totwidth = totwidth + 
			int(real(gchwid[i]) / real(FONT_WIDTH) * cw)
		} else {
		    totwidth = totwidth + 
			int(real(chrwid[i]) / real(FONT_WIDTH) * cw)
		}

		out[op] = text[ip]
		op = op + 1
		totlen = totlen + 1
	    }
	}

	# Terminate last segment and add null segment.
	out[op] = EOS
	out[op+1] = EOS

	return (totlen)
end


# STX_PARAMETERS -- Set the text drawing parameters, i.e., the coordinates
# of the lower left corner of the first character to be drawn, the spacing
# between characters, and the polytext flag.  Input consists of the coords
# of the text string, the length of the string, and the text attributes
# defining the character size, justification in X and Y of the coordinates,
# and orientation of the string.  All coordinates are in GKI units.

procedure stx_parameters (xc, yc, totlen, totwidth, x0, y0, dx, dy, polytext,
   orien)

int	xc, yc			# coordinates at which string is to be drawn
int	totlen			# number of characters to be drawn
int	totwidth		# width of characters to be drawn
int	x0, y0			# lower left corner of first char to be drawn
int	dx, dy			# step in X and Y between characters
int	polytext		# OK to output text segment all at once
int	orien			# rotation angle of characters

pointer	tx
int	up, path
real	dir, sz, ch, cw, cosv, sinv, space
real	xsize, ysize, xvlen, yvlen, xu, yu, xv, yv, p, q
include	"sgi.com"

begin
	tx = SGI_TXAP(g_kt)

	# Get character sizes in GKI coords.
	sz = GKI_UNPACKREAL (TX_SIZE(tx))
	ch = SGI_CHARHEIGHT(g_kt,1) * sz
	cw = SGI_CHARWIDTH(g_kt,1) * sz

	# Compute the character rotation angle.  This is independent of the
	# direction in which characters are drawn.  A character up vector of
	# 90 degrees (normal) corresponds to a rotation angle of zero.

	up = TX_UP(tx)
	orien = up - 90

	# Determine the direction in which characters are to be plotted.
	# This depends on both the character up vector and the path, which
	# is defined relative to the up vector.

	path = TX_PATH(tx)
	switch (path) {
	case GT_UP:
	    dir = up
	case GT_DOWN:
	    dir = up - 180
	case GT_LEFT:
	    dir = up + 90
	default:		# GT_NORMAL, GT_RIGHT
	    dir = up - 90
	}

	# ------- DX, DY ---------
	# Convert the direction vector into the step size between characters.
	# Note CW and CH are in GKI coordinates, hence DX and DY are too.
	# Additional spacing of some fraction of the character size is used
	# if TX_SPACING is nonzero.

	dir = -DEGTORAD(dir)
	cosv = cos (dir)
	sinv = sin (dir)

	# Correct for spacing (unrotated).
	space = (1.0 + TX_SPACING(tx))
	if (path == GT_UP || path == GT_DOWN)
	    p = ch * space
	else
	    p = cw * space
	q = 0

	# Correct for rotation.
	dx =  p * cosv + q * sinv
	dy = -p * sinv + q * cosv

	# ------- XU, YU ---------
	# Determine the coordinates of the center of the first character req'd
	# to justify the string, assuming dimensionless characters spaced on
	# centers DX,DY apart.

	#xvlen = dx * (totlen - 1)
	if (dx > 0)
	    xvlen = totwidth - dx 
	else
	    xvlen = 0
	yvlen = dy * (totlen - 1)

	switch (TX_HJUSTIFY(tx)) {
	case GT_CENTER:
	    xu = - (xvlen / 2.0)
	case GT_RIGHT:
	    # If right justify and drawing to the left, no offset req'd.
	    if (xvlen < 0)
		xu = 0
	    else
		xu = -xvlen
	default:		# GT_LEFT, GT_NORMAL
	    # If left justify and drawing to the left, full offset right req'd.
	    if (xvlen < 0)
		xu = -xvlen
	    else
		xu = 0
	}

	switch (TX_VJUSTIFY(tx)) {
	case GT_CENTER:
	    yu = - (yvlen / 2.0)
	case GT_TOP:
	    # If top justify and drawing downward, no offset req'd.
	    if (yvlen < 0)
		yu = 0
	    else
		yu = -yvlen
	default:		# GT_BOTTOM, GT_NORMAL
	    # If bottom justify and drawing downward, full offset up req'd.
	    if (yvlen < 0)
		yu = -yvlen
	    else
		yu = 0
	}

	# ------- XV, YV ---------
	# Compute the offset from the center of a single character required
	# to justify that character, given a particular character up vector.
	# (This could be combined with the above case but is clearer if
	# treated separately.)

	p = -DEGTORAD(orien)
	cosv = cos(p)
	sinv = sin(p)

	# Compute the rotated character in size X and Y.
	xsize = abs ( cw * cosv + ch * sinv)
	ysize = abs (-cw * sinv + ch * cosv)

	switch (TX_HJUSTIFY(tx)) {
	case GT_CENTER:
	    xv = 0
	case GT_RIGHT:
	    xv = - (xsize / 2.0)
	default:		# GT_LEFT, GT_NORMAL
	    xv = xsize / 2
	}

	switch (TX_VJUSTIFY(tx)) {
	case GT_CENTER:
	    yv = 0
	case GT_TOP:
	    yv = - (ysize / 2.0)
	default:		# GT_BOTTOM, GT_NORMAL
	    yv = ysize / 2
	}

	# ------- X0, Y0 ---------
	# The center coordinates of the first character to be drawn are given
	# by the reference position plus the string justification vector plus
	# the character justification vector.

	x0 = xc + xu + xv
	y0 = yc + yu + yv

	# The character drawing primitive requires the coordinates of the
	# lower left corner of the character (irrespective of orientation).
	# Compute the vector from the center of a character to the lower left
	# corner of a character, rotate to the given orientation, and correct
	# the starting coordinates by addition of this vector.

	p = - (cw / 2.0)
	q = - (ch / 2.0)

	x0 = x0 + ( p * cosv + q * sinv)
	y0 = y0 + (-p * sinv + q * cosv)

	# ------- POLYTEXT ---------
	# Set the polytext flag.  Polytext output is possible only if chars
	# are to be drawn to the right with no extra spacing between chars.

	if (abs(dy) == 0 && dx == cw)
	    polytext = YES
	else
	    polytext = NO
end

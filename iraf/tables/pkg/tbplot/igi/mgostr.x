include <gset.h>
include <math.h>
include <mii.h>
include "igi.h"

define	FONT_CHAR	512
define	NUM_STROKE	17186
define	LEFT_ADJUST	-13
define	RIGHT_ADJUST	13

# MGOSTR -- Draw a text string with optional embedded format escape 
# characters to specify fonts, superscripts, or subscripts.

## 17 June 1992  Modify gio text drawing, giostr(), implement fontset.  ZGL
## 13 August 1992  Add kludge to giostr() to fix justification at
##                 different angles.  ZGL

# \\ - set mode for rest of string
# \  - set mode for next char only
# \r - roman font
# \g - greek font
# \s - script font
# \t - tiny font
# \f - fixed roman font
# \i - toggle italics
# \u - superscript
# \d - subscript
# \b - backspace
# \e - end string

procedure mgostr (igs, xpos, ypos, text, size, angle, just, fontset)

pointer	igs		# igi structure
real	xpos, ypos	# WCS position of text
char	text[ARB]	# Text
real	size		# Text size in units of charsize
real	angle
int	just		# Justification code
int	fontset		# Text font set

pointer	igps		# Parameters sub-structure
pointer	gp		# Graphics descriptor
real	base		# Base NDC character size
real	supfrac, slant

real	wl, wr, wb, wt
real	vl, vr, vb, vt
int	xsign, ysign
real	slength, sheight
real	dx, dy
real	xp, yp

begin
	if (IS_INDEF(xpos) || IS_INDEF(ypos)) {
	    if (PRINT_ERROR(igs) == YES) {
		call eprintf ("igi: no current pen position is defined. ")
		PRINT_ERROR(igs) = NO
	    }
	    return
	}
	
	gp = GIO_GP(igs)

	call ggview (gp, vl, vr, vb, vt)
	call ggwind (gp, wl, wr, wb, wt)

	# Don't clip at viewport boundary;  allow labels outside axes
	call gseti (gp, G_CLIP, NO)

	if (fontset == GIO_FONTS || fontset == HARD_FONTS) {
	    # Use GIO (possibly hardware) characters
	    call giostr (gp, text, xpos, ypos, just, size, angle)
	    return

	} else {
	    # Use igi font set
	    igps    = PLOT_PARMS(igs)
	    base    = MG_CHARSIZE(igps)
	    supfrac = MG_SUPFRAC(igps)
	    slant   = MG_SLANT(igps)

	    # Adjust viewport for unity aspect ratio regardless of device
	    call adjuar (gp, xpos, ypos, xp, yp)

	    call igstrl (text, size, base, supfrac, slant, slength, sheight)

	    # Justification
	    #    7  8  9
	    #    4  5  6
	    #    1  2  3
	    xsign = mod (just - 1, 3) - 1
	    ysign = (just - 1) / 3 - 1
	    dx = (xsign - 1) * 0.5 * slength
	    dy = ysign * 0.5 * sheight
	    xp = xp + dx * cos (DEGTORAD(angle)) - dy * sin (DEGTORAD(angle))
	    yp = yp + dx * sin (DEGTORAD(angle)) + dy * cos (DEGTORAD(angle))

	    call igistr (gp, xp, yp, text, size, angle, base, supfrac, slant)
	}

	# Go back to the original WCS
	call gsview (gp, vl, vr, vb, vt)
	call gswind (gp, wl, wr, wb, wt)
end


procedure giostr (gp, text, xpos, ypos, just, size, angle)

pointer	gp
char	text[ARB]
real	xpos, ypos
int	just
real	angle
real	size

int	ajust
int	ji
real	up
char	vj, hj
char	format[SZ_LINE]

int	jm[8,9]
data	jm / 1, 2, 3, 6, 9, 8, 7, 4,
	     2, 3, 6, 9, 8, 7, 4, 1,
	     3, 6, 9, 8, 7, 4, 1, 2,
	     4, 1, 2, 3, 6, 9, 8, 7,
	     5, 5, 5, 5, 5, 5, 5, 5,
	     6, 9, 8, 7, 4, 1, 2, 3,
	     7, 4, 1, 2, 3, 6, 9, 8,
	     8, 7, 4, 1, 2, 3, 6, 9,
	     9, 8, 7, 4, 1, 2, 3, 6 /

begin
	# Justification
	# 7  8  9
	# 4  5  6
	# 1  2  3

	# Adjust (kludge) for discrepency between gio and igi
	# justification conventions;  map to new justification
	# This works exactly for the cardinal angles, and
	# sort of works for many combinations of other angles and
	# justifications, but not all

	ji = mod (int ((angle + 22.5) / 45.0), 8) + 1
	ajust = jm[ji,just]

	switch (ajust) {
	case 1:
	    # top right
	    hj = 'r';  vj = 't'
	case 2:
	    # top center
	    hj = 'c';  vj = 't'
	case 3:
	    # top left
	    hj = 'l';  vj = 't'
	case 4:
	    # center right
	    hj = 'r';  vj = 'c'
	case 5:
	    # center center
	    hj = 'c';  vj = 'c'
	case 6:
	    # center left
	    hj = 'l';  vj = 'c'
	case 7:
	    # bottom right
	    hj = 'r';  vj = 'b'
	case 8:
	    # bottom center
	    hj = 'c';  vj = 'b'
	case 9:
	    # bottom left
	    hj = 'l';  vj = 'b'
	}

	# Character up vector
	up = angle + 90.0

	call sprintf (format, SZ_LINE, "h=%c;v=%c;u=%f;s=%f;q=h")
	    call pargc (hj)
	    call pargc (vj)
	    call pargr (up)
	    call pargr (size)

	call gtext (gp, xpos, ypos, text, format)
end


procedure igistr (gp, xpos, ypos, text, size, angle, base, supfrac, slant)

pointer	gp
real	xpos, ypos
char	text[ARB]
real	size		# Text size in units of charsize
real	angle
real	base		# Base NDC character size
real	supfrac, slant

bool	draw
real	slength, sheight

begin
	draw = true
	call digstr (gp, xpos, ypos, text, size, angle, 
	    base, supfrac, slant, draw, 
	    slength, sheight)
end


procedure igstrl (text, size, base, supfrac, slant, slength, sheight)

char	text[ARB]
real	size		# Text size in units of charsize
real	base		# Base NDC character size
real	supfrac, slant
real	slength, sheight

pointer	gp
real	xpos, ypos
real	angle
bool	draw

begin
	gp    = NULL
	xpos  = 0.0
	ypos  = 0.0
	angle = 0.0
	draw  = false

	call digstr (gp, xpos, ypos, text, size, angle, 
	    base, supfrac, slant, draw, 
	    slength, sheight)

	sheight = size * base
end


procedure digstr (gp, xpos, ypos, text, size, angle, 
	base, supfrac, slant, draw, 
	slength, sheight)

pointer	gp
real	xpos, ypos
char	text[ARB]
real	size		# Text size in units of charsize
real	angle
real	base		# Base NDC character size
real	supfrac, slant
bool	draw
real	slength, sheight

bool	indata
data	indata/false/
bool	relo
bool	italic, itsave
int	font
int	achar, jchar
int	i, j
real	xa
real	xp, yp
real	x0, y0
real	xc, yc
real	x, y
int	ix, iy
real	co, si
int	icomm
int	ioffset
int	isuper
real	supfactor, supshift
int	iosave
int	issave
real	fasave
real	shsave
short	iladj, iradj
real	tsize
bool	fixedf, ffsave

short	nstroke[FONT_CHAR]
short	ladj[FONT_CHAR], radj[FONT_CHAR]
int	fpoint[FONT_CHAR]
short	strokes[NUM_STROKE]

begin
	if (!indata) {
	    # Read the fonts file
	    call rdfont (nstroke, ladj, radj, fpoint, strokes, indata)
	    if (!indata) 
		return
	}

	tsize = size * base / 21.0

	co = cos (DEGTORAD (angle))
	si = sin (DEGTORAD (angle))

	font      = ROMAN_FONT
	slength   = 0	    
	icomm     = 0
	italic    = false
	ioffset   = 0
	isuper    = 0
	supfactor = 1.0
	supshift  = 0.0
	fixedf    = false

	itsave = false
	iosave = 0
	issave = 0
	fasave = 1.0
	shsave = 0.0
	ffsave = false

	xp = xpos
	yp = ypos
	x0 = xp
	y0 = yp

	for (j = 1; text[j] != EOS; j = j + 1) {
	    # For each character in string
	    if (text[j] == TEXT_ESCAPE)
		# Format command
		icomm = icomm + 1
	    else if (icomm > 0) {
		switch (text[j]) {
		case 'r', 'R':
		    ioffset = 0
		    font = ROMAN_FONT
		case 'g', 'G':
		    ioffset = 128
		    font = GREEK_FONT
		case 's', 'S':
		    ioffset = 256
		    font = SCRIPT_FONT
		case 't', 'T':
		    ioffset = 384
		    font = TINY_FONT
		case 'f', 'F':
		    fixedf = !fixedf
		case 'i', 'I':
		    italic = !italic
		case 'u', 'U':
		    isuper = isuper + 1
		    supshift = supshift + 16.0 * supfactor
		    supfactor = supfrac ** abs (isuper)
		case 'd', 'D':
		    isuper = isuper - 1
		    supfactor = supfrac ** abs (isuper)
		    supshift = supshift - 16.0 * supfactor
		case 'b', 'B':
		    x0 = x0 - co * xa
		    y0 = y0 - si * xa
		    next
		case 'e', 'E':
		    break
		}

		if (icomm > 1) {
		    # Escape rest of string
		    itsave = italic
		    iosave = ioffset
		    issave = isuper
		    fasave = supfactor
		    shsave = supshift
		    ffsave = fixedf
		}
		icomm = 0
	    } else {
		# Printable character
		jchar = text[j] + 1 + ioffset
		achar = text[j] 

		if (fixedf && font != TINY_FONT) {
		    iladj = LEFT_ADJUST
		    iradj = RIGHT_ADJUST
		} else {
		    iladj = ladj[jchar]
		    iradj = radj[jchar]
		}

		relo = true
		do i = 0, nstroke[jchar] - 1 {
		    # For each stroke endpoint
		    ix = strokes[fpoint[jchar]+2*i]
		    if (ix == 31)
			# New polyline
			relo = true
		    else {
			# Next stroke
			iy = strokes[fpoint[jchar]+2*i+1]
			x = ix - iladj
			if (italic) 
			    x = x + slant * (iy + 9)
			x  = tsize * supfactor * x
			y  = tsize * (iy * supfactor + supshift)
			xc = co*x - si*y + x0
			yc = si*x + co*y + y0

			if (relo)
			    relo = false
			else if (draw)
			    call gline (gp, xp, yp, xc, yc)

			xp = xc
			yp = yc
		    }
		}

		xa = tsize * (iradj - iladj) * supfactor
		slength = slength + xa

		x0 = x0 + co * xa
		y0 = y0 + si * xa

		italic    = itsave
		ioffset   = iosave
		isuper    = issave
		supfactor = fasave
		supshift  = shsave
	    }
	}
end


procedure rdfont (nstroke, ladj, radj, fpoint, strokes, indata)

# RDFONT -- Read the font stroke file

short	nstroke[FONT_CHAR]
short	ladj[FONT_CHAR]
short	radj[FONT_CHAR]
int	fpoint[FONT_CHAR]
short	strokes[NUM_STROKE]
bool	indata

int	ffd
int	nchar
int	ndata
char	errmsg[SZ_LINE]

string	font_file	"tables$base/miifont.mip"

int	access(), open()

begin
	if (access (font_file, 0, BINARY_FILE) == NO) {
	    call sprintf (errmsg, SZ_LINE, "Cannot find font file, %s")
		call pargstr (font_file)
	    call error (0, errmsg)
	}

	# Binary font file found;  open it
	ffd = open (font_file, READ_ONLY, BINARY_FILE)

	call miigeti (ffd, nchar, 1)

	call miigeti (ffd, ndata, 1)

	call miigets (ffd, nstroke, FONT_CHAR)

	call miigets (ffd, ladj, FONT_CHAR)

	call miigets (ffd, radj, FONT_CHAR)

	call miigeti (ffd, fpoint, FONT_CHAR)

	call miigets (ffd, strokes, NUM_STROKE)

	call close (ffd)

	indata = true
end


procedure miigeti (ffd, sval, nval)

int	ffd
int	sval[ARB]
int	nval

pointer	miibuf
int	nch

int	miipksize(), read()

begin
	nch = miipksize (nval, MII_LONG)
	call malloc (miibuf, nch, TY_CHAR)

	if (read (ffd, Memc[miibuf], nch) == EOF)
	    call error (0, "Error reading font file\n")

	call miiupk (Memc[miibuf], sval, nval, MII_LONG, TY_INT)

	call mfree (miibuf, TY_CHAR)
end


procedure miigets (ffd, sval, nval)

int	ffd
short	sval[ARB]
int	nval

pointer	miibuf
int	nch

int	miipksize(), read()

begin
	nch = miipksize (nval, MII_SHORT)
	call malloc (miibuf, nch, TY_CHAR)

	if (read (ffd, Memc[miibuf], nch) == EOF)
	    call error (0, "Error reading font file\n")

	call miiupk (Memc[miibuf], sval, nval, MII_SHORT, TY_SHORT)

	call mfree (miibuf, TY_CHAR)
end



procedure adjuar (gp, xpos, ypos, xp, yp)

pointer	gp
real	xpos, ypos
real	xp, yp

real	ar, xs, ys
int	wcs
real	wl, wr, wb, wt

int	gstati()
real	ggetr()
bool	fp_equalr()

begin
	# Device aspect ratio
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")

	if (fp_equalr (xs, 0.0) || fp_equalr (xs, 0.0))
	    ar = 1.0
	else
	    ar = ys / xs

	if (fp_equalr (ar, 1.0) || fp_equalr (ar+1.0, 1.0)) {
	    # Square
	    wl = 0.0
	    wr = 1.0
	    wb = 0.0
	    wt = 1.0
	} else if (ar > 1.0) {
	    # Portrait
	    wl = 0.0
	    wr = 1.0 / ar
	    wb = 0.0
	    wt = 1.0
	} else if (ar < 1.0) {
	    # Landscape
	    wl = 0.0
	    wr = 1.0
	    wb = 0.0
	    wt = ar
	}

	wcs = gstati (gp, G_WCS)
	call ig_gctran (gp, xpos, ypos, xp, yp, wcs, 0)
	call gsview (gp, 0.0, 1.0, 0.0, 1.0)
	call gswind (gp, wl, wr, wb, wt)
	call ig_gctran (gp, xp, yp, xp, yp, 0, wcs)
end

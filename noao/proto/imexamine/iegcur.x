# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"imexam.h"
 
# IE_GCUR -- Get IMEXAM cursor value.
# This is an interface between the standard cursor input and IMEXAM.
# It reads the appropriate cursor, determines the image index or frame
# type, makes the appropriate default coordinate conversions when using
# graphics cursor input, and gets any further cursor reads needed.
# Missing coordinates default to the last coordinates.
 
int procedure ie_gcur (ie, curtype, x, y, key, strval, maxch)
 
pointer	ie			#I IMEXAM structure
int	curtype			#I cursor type (0=image, 1=graphics, 2=text)
real	x, y			#O cursor position
int	key			#O keystroke value of cursor event
char	strval[ARB]		#O string value, if any
int	maxch			#I max chars out
 
char	ch
real	x1, y1, x2, y2, r, cosa, sina
int	temp, k[2], nitems, wcs, ip, i

char	clgetc()
int	clgcur(), imd_gcur(), ctor(), cctoc()
errchk	clgcur, imd_gcur
 
begin
	# Save last cursor value.
	x1 = x;  y1 = y
	strval[1] = EOS
	k[1] = clgetc ("defkey")

	# Get one or more cursor values from the desired cursor parameter.
	# Check for missing coordinates and substitute the last value.

	do i = 1, 2 {
	    switch (curtype) {
	    case 'i':
		nitems = imd_gcur ("imagecur", x, y, wcs, k[i], strval, maxch)
		if (IS_INDEF(x))
		    x = x1
		if (IS_INDEF(y))
		    y = y1
		if (wcs >= 100)
		    IE_NEWFRAME(ie) = wcs / 100
		else if (wcs >= 1)
		    IE_NEWFRAME(ie) = wcs
		if (IE_DFRAME(ie) <= 0)
		    IE_DFRAME(ie) = IE_NEWFRAME(ie)

	    case 'g':
		nitems = clgcur ("graphcur", x, y, wcs, k[i], strval, maxch)
		if (IS_INDEF(x))
		    x = x1
		if (IS_INDEF(y))
		    y = y1

		# Make any needed default coordinate conversions from the
		# graphic coordinates.

		switch (IE_GTYPE(ie)) {
		case 'c':	# Column plot
		    y = x
		    x = IE_X1(ie)
		case 'e':	# Contour plot
		case 'l':	# Line plot
		    y = IE_Y1(ie)
		case 'r':	# Radial profile plot
		    x = IE_X1(ie)
		    y = IE_Y1(ie)
		case 'h', 's':	# Surface plot
		    x = IE_X1(ie)
		    y = IE_Y1(ie)
		case 'u':	# Vector plot
		    y = x * sina + (IE_Y1(ie) + IE_Y2(ie)) / 2
		    x = x * cosa + (IE_X1(ie) + IE_X2(ie)) / 2
		case 'v':	# Vector plot
		    y = x * sina + IE_Y1(ie)
		    x = x * cosa + IE_X1(ie)
		}
	    }

	    key = k[1]
	    switch (key) {
	    case 'v', 'u':
		if (i == 1) {
		    x1 = x
		    y1 = y
		    call printf ("again:")
		} else {
		    x2 = x
		    y2 = y
		    r = sqrt (real ((y2-y1)**2 + (x2-x1)**2))
		    if (r > 0.) {
		        cosa = (x2 - x1) / r
		        sina = (y2 - y1) / r
		    } else {
		        cosa = 0.
		        sina = 0.
		    }
		    call printf ("\n")
		    switch (key) {
		    case 'v':
		        x = x1
		        y = y1
		    case 'u':
			x = 2 * x1 - x2
			y = 2 * y1 - y2
		    }
		    IE_X2(ie) = x2
		    IE_Y2(ie) = y2
		    break
		}
	    case 'b':
		if (i == 1) {
		    IE_IX1(ie) = x + 0.5
		    IE_IY1(ie) = y + 0.5
		    call printf ("again:")
		} else {
		    IE_IX2(ie) = x + 0.5
		    IE_IY2(ie) = y + 0.5
		    call printf ("\n")
		    temp = IE_IX1(ie)
		    IE_IX1(ie) = min (IE_IX1(ie), IE_IX2(ie))
		    IE_IX2(ie) = max (temp, IE_IX2(ie))
		    temp = IE_IY1(ie)
		    IE_IY1(ie) = min (IE_IY1(ie), IE_IY2(ie))
		    IE_IY2(ie) = max (temp, IE_IY2(ie))
		    break
		}
	    default:
		break
	    }
	}

	# Map numeric colon sequences (: x [y] key strval) to make them appear
	# as ordinary "x y key" type cursor reads.  This makes it possible for
	# the user to access any command using typed in rather than positional
	# cursor coordinates.  Special treatment is also given to the syntax
	# ":lN" and ":cN", provided for compatibility with IMPLOT for simple
	# line and column plots.

	if (key == ':') {
	    for (ip=1;  IS_WHITE(strval[ip]);  ip=ip+1)
		;
	    if (IS_DIGIT(strval[ip])) {
		if (ctor (strval, ip, x) <= 0)
		    ;
		if (ctor (strval, ip, y) <= 0)
		    y = x
		for (;  IS_WHITE(strval[ip]);  ip=ip+1)
		    ;
		if (cctoc (strval, ip, ch) > 0)
		    key = ch
		call strcpy (strval[ip], strval, maxch)

	    } else if (strval[ip] == 'l' && IS_DIGIT(strval[ip+1])) {
		ip = ip + 1
		if (ctor (strval, ip, x) > 0) {
		    y = x
		    key = 'l'
		}
	    } else if (strval[ip] == 'c' && IS_DIGIT(strval[ip+1])) {
		ip = ip + 1
		if (ctor (strval, ip, x) > 0) {
		    y = x
		    key = 'c'
		}
	    }
	}
 
	return (nitems)
end

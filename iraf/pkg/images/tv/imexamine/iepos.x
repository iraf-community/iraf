# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<math.h>
include	"imexam.h"
 
# IE_POS -- Print cursor position and pixel value or set new origin.
# If the origin is not (0,0) print additional fields.
 
procedure ie_pos (ie, x, y, key)
 
pointer	ie			# IMEXAM structure
real	x, y			# Center of box
int	key			# Key ('x' positions, 'y' origin)
 
pointer	im, data
real	dx, dy, r, t, wx, wy, xo, yo
int	x1, x2, y1, y2
pointer	ie_gimage(), ie_gdata()
 
begin
	switch (key) {
	case 'x':	# Print position and pixel value
	    iferr (im = ie_gimage (ie, NO)) {
	        call erract (EA_WARN)
	        return
	    }

	    x1 = x + 0.5
	    x2 = x + 0.5
	    y1 = y + 0.5
	    y2 = y + 0.5
	    iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	        call erract (EA_WARN)
	        return
	    }

	    call printf ("%7.2f %7.2f %7g")
	        call pargr (x)
	        call pargr (y)
	        call pargr (Memr[data])

	    # Print additional fields
	    if (IE_XORIGIN(ie) != 0. || IE_YORIGIN(ie) != 0.) {
	        dx = x - IE_XORIGIN(ie)
	        dy = y - IE_YORIGIN(ie)
	        r = sqrt (dx * dx + dy * dy)
	        t = mod (360. + RADTODEG (atan2 (dy, dx)), 360.)
	        call printf (" %7.f %7.2f %7.2f %7.2f %7.2f %5.1f")
		    call pargr (IE_XORIGIN(ie))
		    call pargr (IE_YORIGIN(ie))
	            call pargr (dx)
	            call pargr (dy)
	            call pargr (r)
	            call pargr (t)
	    }
	    call printf ("\n")
	case 'y':	# Set new origin
	    IE_XORIGIN(ie) = x
	    IE_YORIGIN(ie) = y
	    call printf ("Origin: %.2f %.2f\n")
		call pargr (IE_XORIGIN(ie))
		call pargr (IE_YORIGIN(ie))
	}

	# Print to logfile if needed.
	if (IE_LOGFD(ie) != NULL) { 
	    switch (key) {
	    case 'x':
	        call fprintf (IE_LOGFD(ie), "%7.2f %7.2f %7g")
	            call pargr (x)
	            call pargr (y)
	            call pargr (Memr[data])
	        if (IE_XORIGIN(ie) != 0. || IE_YORIGIN(ie) != 0.) {
	            dx = x - IE_XORIGIN(ie)
	            dy = y - IE_YORIGIN(ie)
	            r = sqrt (dx * dx + dy * dy)
	            t = mod (360. + RADTODEG (atan2 (dy, dx)), 360.)
	            call fprintf (IE_LOGFD(ie),
			" %7.f %7.2f %7.2f %7.2f %7.2f %5.1f")
		        call pargr (IE_XORIGIN(ie))
		        call pargr (IE_YORIGIN(ie))
	                call pargr (dx)
	                call pargr (dy)
	                call pargr (r)
	                call pargr (t)
	        }
	        call fprintf (IE_LOGFD(ie), "\n")
	    case 'y':	# Set new origin
		IE_XORIGIN(ie) = x
		IE_YORIGIN(ie) = y
		call fprintf (IE_LOGFD(ie), "Origin: %.2f %.2f\n")
		    call pargr (IE_XORIGIN(ie))
		    call pargr (IE_YORIGIN(ie))
	    }
	}

	# Print in WCS if necessary.
	call ie_mwctran (ie, x, y, wx, wy)
	if (x == wx && y == wy)
	    return
	call ie_mwctran (ie, IE_XORIGIN(ie), IE_YORIGIN(ie), xo, yo)

	switch (key) {
	case 'x':	# Print position and pixel value
	    if (IE_XFORMAT(ie) == '%')
		call printf (IE_XFORMAT(ie))
	    else
		call printf ("%7g")
	        call pargr (wx)
	    call printf (" ")
	    if (IE_YFORMAT(ie) == '%')
		call printf (IE_YFORMAT(ie))
	    else
		call printf ("%7g")
	        call pargr (wy)
	    call printf (" %7g")
	        call pargr (Memr[data])

	    # Print additional fields
	    if (IE_XORIGIN(ie) != 0. || IE_YORIGIN(ie) != 0.) {
	        dx = wx - xo
	        dy = wy - yo
	        r = sqrt (dx * dx + dy * dy)
	        t = mod (360. + RADTODEG (atan2 (dy, dx)), 360.)
	        call printf (" %7g %7g %7g %7g %7g %5.1f")
		    call pargr (xo)
		    call pargr (yo)
	            call pargr (dx)
	            call pargr (dy)
	            call pargr (r)
	            call pargr (t)
	    }
	    call printf ("\n")
	case 'y':	# Set new origin
	    call printf ("Origin: %7g %7g\n")
		call pargr (xo)
		call pargr (yo)
	}

	# Print to logfile if needed.
	if (IE_LOGFD(ie) != NULL) { 
	    switch (key) {
	    case 'x':
		if (IE_XFORMAT(ie) == '%')
		    call fprintf (IE_LOGFD(ie), IE_XFORMAT(ie))
		else
		    call fprintf (IE_LOGFD(ie), "%7g")
		    call pargr (wx)
		call fprintf (IE_LOGFD(ie), " ")
		if (IE_YFORMAT(ie) == '%')
		    call fprintf (IE_LOGFD(ie), IE_YFORMAT(ie))
		else
		    call fprintf (IE_LOGFD(ie), "%7g")
		    call pargr (wy)
		call fprintf (IE_LOGFD(ie), " %7g")
		    call pargr (Memr[data])

	        if (IE_XORIGIN(ie) != 0. || IE_YORIGIN(ie) != 0.) {
	            dx = wx - xo
	            dy = wy - yo
	            r = sqrt (dx * dx + dy * dy)
	            t = mod (360. + RADTODEG (atan2 (dy, dx)), 360.)
	            call fprintf (IE_LOGFD(ie),
			" %7g %7g %7g %7g %7g %5.1f")
		        call pargr (xo)
		        call pargr (yo)
	                call pargr (dx)
	                call pargr (dy)
	                call pargr (r)
	                call pargr (t)
	        }
	        call fprintf (IE_LOGFD(ie), "\n")
	    case 'y':	# Set new origin
		call fprintf (IE_LOGFD(ie), "Origin: %7g %7g\n")
		    call pargr (xo)
		    call pargr (yo)
	    }
	}
end

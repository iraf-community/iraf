# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<ttset.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"
include	"grc.h"

define	SZ_CHARCON	5
define	MARKLEN		0.01

# Cursor step algorithm parameters.

define	MAX_STEP	0.1		# max cursor step size, cursor motions
define	MIN_STEP	0.002		# min cursor step size, cursor motions
define	LARGER_STEP	2.0		# factor by which step size is increased
define	SMALLER_STEP	0.5		# factor by step size is decreased
define	NSTEP		2		# number of steps before larger step
define	MANUAL_STEP	5.0		# gear ratio for F/V cursor control
define	SLOW		1		# for fast/slow algorithm
define	FAST		2

# Zoom parameters.

define	X_ZOOMFACTOR	0.5		# zoom factors
define	Y_ZOOMFACTOR	0.5
#define	X_ZOOMFACTOR	0.666		# zoom factors
#define	Y_ZOOMFACTOR	0.666

# Roam factors.

define	X_ROAM		0.333		# fraction of the current window
define	Y_ROAM		0.333		# fraction of the current window


# RCURSOR -- Read the position of a cursor.  This is the main entry point to
# cursor mode/cursor input from the CL; we are called by the QUERY procedure
# of the CL when a cursor type parameter is read.  The cursor position is
# returned as a string of the form
#
#	x y wcs key stringval
#
# where the "stringval" field may be absent if not appropriate for a given key.
# If EOF is returned the cursor value string is undefined.

int procedure rcursor (stream, outstr, maxch)

int	stream			# graphics stream
char	outstr[ARB]		# encoded cursor value (output)
int	maxch

bool	cminit
int	xroam[9], yroam[9]
pointer	rc, tr, sp, lbuf, ip
char	charcon[SZ_CHARCON], ch
real	x1, x2, y1, y2, xt, yt, v[10]
real	lx1, lx2, ly1, ly2, aspect_ratio
real	x, y, rx, ry, xw, yw, dx, dy, xc, yc
int	junk, key, nukey, last_zoom, i, wcs, ppos, ucasein, raster

bool	ttygetb()
pointer	grc_open()
int	envfind(), ctocc(), oscmd(), gtr_readcursor(), grc_readtty()
int	grc_cursor(), grc_command(), grc_selectwcs(), grc_mapkey(), ttstati()
real	ttygetr()

errchk	grc_text, grc_readtty, grc_writecursor
errchk	grc_init, grc_open, grc_command, grc_cursor, grc_message
errchk	grc_readcursor, grc_mapkey, grc_redraw, envfind

data	xroam /1,0,-1,1,0,-1,1,0,-1/
data	yroam /1,1,1,0,0,0,-1,-1,-1/
data	rc /NULL/
define	done_ 91
define	coloncmd_ 92

begin
	call smark (sp)
	call salloc (lbuf, SZ_COMMAND, TY_CHAR)

	# Allocate and initialize the RCURSOR descriptor.
	if (rc == NULL) {
	    call grc_init (rc)
	    cminit = true
	} else
	    cminit = false

	# Open or reopen the graphics kernel.
	tr = grc_open ("", APPEND, stream, rc)

	# Process CMINIT command string, if present in environment.  This is
	# only done once.

	if (cminit) {
	    if (envfind ("cminit", Memc[lbuf], SZ_COMMAND) > 0) {
		ip = lbuf
		while (IS_WHITE(Memc[ip]) || Memc[ip] == '.')
		    ip = ip + 1
		junk = grc_command (rc, stream, 0.,0.,0,0.,0., Memc[ip])
	    }
	    cminit = false
	}

	# If the graphics device does not permit input, i.e., does not have
	# a cursor, return EOF.

	if (!ttygetb (TR_TTY(tr), "in")) {
	    x = 0; y = 0
	    key = EOF
	    goto done_
	}

	# Determine if input keys are to be mapped to lower case by default,
	# i.e., ucasein mode has been set for the terminal driver.

	ucasein = ttstati (STDIN, TT_UCASEIN)

	last_zoom = 3
	ppos = NO

	# Enter cursor mode loop.  The loop terminates when a non cursor mode
	# keystroke is typed.

	while (grc_cursor (rc, stream, key,x,y, raster,rx,ry, ppos) != EOF) {
	    Memc[lbuf] = EOS

	    # As a rule, no processing is performed on escaped keys.  The only
	    # exception is when ucasein mode is set in the terminal driver,
	    # causing upper case input to be mapped to lower case.  This mapping
	    # is disabled in a raw mode cursor read, hence we must perform the
	    # mapping explicitly here, returning a lower case key to the
	    # applications program.  Unescaped upper case input keystrokes will
	    # be intercepted by cursor mode when ucasein mode is in effect.

	    if (key == '\\') {
		junk = gtr_readcursor (stream, key, x, y, raster, rx, ry)
		if (ucasein == YES && IS_UPPER(key))
		    key = TO_LOWER (key)
		break
	    }

	    # Map keystroke.  If the keystroke maps to a null value the key
	    # is not recognized as a cursor mode keystroke and we exit.

	    if (grc_mapkey (rc, key, nukey) == NULL)
		break

	    switch (nukey) {
	    case 'M':
		# Move the feature under the cursor to the center of the
		# screen without changing the scaling.

		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, x1, x2, y1, y2)
		xw = (x2 - x1) / 2.
		yw = (y2 - y1) / 2.
		call gtr_ptran (stream, xc-xw, xc+xw, yc-yw, yc+yw)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call grc_restorecurpos (stream, xc, yc)

	    case 'Z':
		# Zoom in in both X and Y.
		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, x1, x2, y1, y2)
		xw = (x2 - x1) * X_ZOOMFACTOR / 2.
		yw = (y2 - y1) * Y_ZOOMFACTOR / 2.
		call gtr_ptran (stream, xc-xw, xc+xw, yc-yw, yc+yw)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call grc_restorecurpos (stream, xc, yc)
		last_zoom = 3

	    case 'X':
		# Zoom in in X.
		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, x1, x2, y1, y2)
		xw = (x2 - x1) * X_ZOOMFACTOR / 2.
		call gtr_ptran (stream, xc-xw, xc+xw, y1, y2)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call grc_restorecurpos (stream, xc, yc)
		last_zoom = 1

	    case 'Y':
		# Zoom in in Y.
		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, x1, x2, y1, y2)
		yw = (y2 - y1) * Y_ZOOMFACTOR / 2.
		call gtr_ptran (stream, x1, x2, yc-yw, yc+yw)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call grc_restorecurpos (stream, xc, yc)
		last_zoom = 2

	    case '>':
		# Zoom in in Y by setting the upper limit of the viewport
		# to the cursor Y position.

		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, lx1, lx2, ly1, ly2)
		call gtr_ptran (stream, lx1, lx2, ly1,  yc)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call gtr_writecursor (stream, x, 0.5)
		last_zoom = 'E'

	    case '<':
		# Zoom in in Y by setting the lower limit of the viewport
		# to the cursor Y position.

		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, lx1, lx2, ly1, ly2)
		call gtr_ptran (stream, lx1, lx2,  yc, ly2)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call gtr_writecursor (stream, x, 0.5)
		last_zoom = 'E'

	    case 'E':
		# Expand by marking corners of new viewport.  If the range is
		# small in either X or Y only the other axis will be expanded.

		call gtr_gtran (stream, lx1, lx2, ly1, ly2)
		call grc_scrtondc (x, y, x1, y1)
		call grc_message (stream, "again:")
		junk = grc_cursor (rc, stream, key,x2,y2, raster,rx,ry, ppos)
		call grc_scrtondc (x2, y2, x2, y2)

		if (x1 > x2)
		    { xt = x2;  x2 = x1;  x1 = xt }
		if (y1 > y2)
		    { yt = y2;  y2 = y1;  y1 = yt }

		if (abs (x1 - x2) < .01)
		    call gtr_ptran (stream, lx1, lx2, y1, y2)
		else if (abs (y1 - y2) < .01)
		    call gtr_ptran (stream, x1, x2, ly1, ly2)
		else
		    call gtr_ptran (stream, x1, x2, y1, y2)

		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call gtr_writecursor (stream, 0.5, 0.5)
		last_zoom = 'E'

	    case 'P':
		# Zoom out.
		call grc_scrtondc (x, y, xc, yc)
		call gtr_gtran (stream, x1, x2, y1, y2)

		if (last_zoom == 'E') {
		    call gtr_ptran (stream, lx1, lx2, ly1, ly2)
		    lx1 = x1; lx2 = x2; ly1 = y1; ly2 = y2
		} else {
		    if (last_zoom == 1 || last_zoom == 3) {
			xw = (x2 - x1) / X_ZOOMFACTOR / 2.
			x1 = xc - xw
			x2 = xc + xw
		    }
		    if (last_zoom == 2 || last_zoom == 3) {
			yw = (y2 - y1) / Y_ZOOMFACTOR / 2.
			y1 = yc - yw
			y2 = yc + yw
		    }
		    call gtr_ptran (stream, x1, x2, y1, y2)
		}

		call grc_redraw (rc, stream, x, y, raster, rx, ry)
		call grc_restorecurpos (stream, xc, yc)

	    case 'W':
		# Select and fix WCS to be used for scr->wcs coordinate
		# transformations.

		call grc_scrtondc (x, y, xc, yc)
		TR_WCS(tr) = grc_selectwcs (tr, raster, xc, yc)

	    case 'C':
		# Running tally of cursor position.
		#if (ppos == NO) {
		#    call grc_pcursor (stream, x, y, raster, rx, ry)
		#    ppos = YES
		#} else {
		#    call grc_message (stream, "\n\n")
		#    ppos = NO
		#}

		call grc_pcursor (stream, x, y, raster, rx, ry)

	    case 'D':
		# Draw a line by marking the endpoints.
		call grc_scrtondc (x, y, v[1], v[2])
		call grc_message (stream, "again:")
		junk = grc_cursor (rc, stream, key,x2,y2, raster,rx,ry, ppos)
		call grc_scrtondc (x2, y2, v[3], v[4])
		call grc_polyline (stream, v, 2)

	    case 'T':
		# Draw a text string.
		if (grc_readtty (stream, "text: ", Memc[lbuf], SZ_COMMAND) <= 0)
		    next
		call grc_scrtondc (x, y, xc, yc)
		call grc_text (stream, xc, yc, Memc[lbuf])

	    case 'A':
		# Draw and label the axes of the viewport.
		call grc_axes (stream, x, y, raster, rx, ry)

	    case 'B':
		# Backup one instruction in the frame buffer.
		call gtr_backup (stream)

	    case 'U':
		# Undo the last frame buffer edit.
		call gtr_undo (stream)

	    case 'R':
		# Redraw the screen.
		call grc_redraw (rc, stream, x, y, raster, rx, ry)

	    case '0':
		# Reset and redraw.
		call gtr_ptran (stream, 0., 1., 0., 1.)
		call gtr_writecursor (stream, .5, .5)
		call grc_redraw (rc, stream, x, y, raster, rx, ry)

	    case '5':
		# Redraw (null roam request).
		call grc_redraw (rc, stream, x, y, raster, rx, ry)

	    case '1','2','3','4','6','7','8','9':
		# Roam.
		i = TO_INTEG (key)
		if (xroam[i] != 0 || yroam[i] != 0) {
		    call gtr_gtran (stream, x1, x2, y1, y2)
		    dx = (x2 - x1) * X_ROAM * xroam[i]
		    dy = (y2 - y1) * Y_ROAM * yroam[i]
		    call gtr_ptran (stream, x1+dx, x2+dx, y1+dy, y2+dy)
		    call grc_redraw (rc, stream, x, y, raster, rx, ry)
		}

	    case ':':
		# Enter a colon command string and terminate cursor mode.

		# Get the string value.
		if (grc_readtty (stream, ":", Memc[lbuf], SZ_COMMAND) <= 0)
		    next

		# All cursor mode commands must begin with a ".".  An osescape
		# begins with an "!".

		if (Memc[lbuf] == '!') {
		    call gtr_page (STDERR, stream)
		    if (oscmd (Memc[lbuf+1], "", "", "") == ERR)
			call fprintf (STDERR, "\7")
		    call gtr_waitpage (STDERR, stream)
		    
		} else if (Memc[lbuf] == '.') {
		    # Save viewport for 'P'.
coloncmd_
		    call gtr_gtran (stream, lx1, lx2, ly1, ly2)
		    last_zoom = 'E'

		    TR_WAITPAGE(tr) = NO
		    if (grc_command (rc, stream, x, y, raster, rx, ry,
			    Memc[lbuf+1]) == EOF) {
			key = EOF
			goto done_
		    }

		    # The following is a no-op for most colon commands.
		    if (TR_WAITPAGE(tr) == YES)
			call gtr_waitpage (STDERR, stream)
		} else
		    break

	    case '=':
		# Shorthand for :.snap.  The latter must be used once to
		# set the plotter device, else the default stdplot device
		# will be used.

		call strcpy (".snap", Memc[lbuf], SZ_COMMAND)
		goto coloncmd_

	    default:
		call fprintf (STDERR, "\007")
	    }
	}

	# Mark the cursor position if markcur enabled.
	if (RC_MARKCUR(rc) == YES && key != EOF) {
	    call grc_scrtondc (x, y, xc, yc)
	    aspect_ratio = ttygetr (TR_TTY(tr), "ar")
	    if (aspect_ratio < .001)
		aspect_ratio = 1.0

	    v[1]  = xc - MARKLEN * aspect_ratio
	    v[2]  = yc
	    v[3]  = xc + MARKLEN * aspect_ratio
	    v[4]  = yc
	    v[5]  = xc
	    v[6]  = yc
	    v[7]  = xc
	    v[8]  = yc - MARKLEN
	    v[9]  = xc
	    v[10] = yc + MARKLEN
	    call grc_polyline (stream, v, 5)
	}

	# Close the workstation, leave graphics mode, position alpha cursor to
	# lower left corner of graphics terminal.

	call grc_close (stream, rc)

	# Encode the cursor value as a string for the CL.
done_
	if (key != EOF) {
	    if (key == ' ')
		call strcpy ("\\40", charcon, SZ_CHARCON)
	    else {
		ch = char (key)
		junk = ctocc (ch, charcon, SZ_CHARCON)
	    }
	    call grc_scrtowcs (stream, x, y, raster, rx, ry, xc, yc, wcs)

	    call sprintf (outstr, maxch, "%g %g %d %s %s\n")
		call pargr (xc)
		call pargr (yc)
		call pargi (wcs)
		call pargstr (charcon)
		call pargstr (Memc[lbuf])
	} else
	    outstr[1] = EOS

	call sfree (sp)
	return (key)
end


# GRC_CURSOR -- Read the position of a cursor in screen coordinates.  Recognizes
# the cursor movement keystrokes H, J, K, and L, exiting only when some other
# keystroke is received.  The cursor movement algorithm is initialized upon
# entry.  Two algorithms are provided for controlling the cursor step size.
# The first algorithm (automatic control) starts with a large initial step
# size.  In the vicinity of a feature the cursor will overshoot the feature
# and the user will step back in the opposite direction, causing the step size
# to be decreased, rapidly converging to the desired position.  Several steps
# in the same direction cause the large step size to be restored.  The second
# algorithm (manual control) uses the F and V keys to directly control the step
# size.

int procedure grc_cursor (rc, stream, key, x, y, raster, rx, ry, ppos)

pointer	rc			#I rcursor descriptor
int	stream			#I graphics stream
int	key			#O keystroke typed
real	x, y			#O cursor screen coordinates
int	raster			#O raster number
real	rx, ry			#O cursor raster coordinates
int	ppos			#I print cursor position flag

int	speed
int	xdir, ydir, nukey
real	xstep, ystep, newx, newy

bool	ttygetb()
pointer	gtr_gtty()
int	gtr_readcursor(), grc_mapkey()
errchk	gtr_readcursor, gtr_writecursor

begin
	# Reset the cursor step size to the default.
	xstep = MAX_STEP
	ystep = MAX_STEP
	xdir  = 0
	ydir  = 0
	speed = 0

	while (gtr_readcursor (stream, key, x, y, raster, rx, ry) != EOF) {
	    if (grc_mapkey (rc, key, nukey) == NULL)
		break

	    newx = x
	    newy = y

	    switch (nukey) {
	    case 'F':
		# Faster.
		xstep = min (MAX_STEP, xstep * MANUAL_STEP)
		ystep = min (MAX_STEP, ystep * MANUAL_STEP)
		speed = FAST

	    case 'V':
		# Slower.
		xstep = max (MIN_STEP, xstep / MANUAL_STEP)
		ystep = max (MIN_STEP, ystep / MANUAL_STEP)
		speed = SLOW

	    case 'H':
		# Step cursor left.
		if (speed == 0)
		    if (xdir < -NSTEP) {
			xstep = MAX_STEP
			xdir = -1
		    } else if (xdir > 0) {
			xstep = max (MIN_STEP, xstep * SMALLER_STEP)
			xdir = -1
		    } else
			xdir = xdir - 1
		newx = newx - xstep
		call gtr_writecursor (stream, newx, newy)

	    case 'J':
		# Step cursor down.
		if (speed == 0)
		    if (ydir < -NSTEP) {
			ystep = MAX_STEP
			ydir = -1
		    } else if (ydir > 0) {
			ystep = max (MIN_STEP, ystep * SMALLER_STEP)
			ydir = -1
		    } else
			ydir = ydir - 1
		newy = newy - ystep
		call gtr_writecursor (stream, newx, newy)

	    case 'K':
		# Step cursor up.
		if (speed == 0)
		    if (ydir > NSTEP) {
			ystep = MAX_STEP
			ydir = 1
		    } else if (ydir < 0) {
			ystep = max (MIN_STEP, ystep * SMALLER_STEP)
			ydir = 1
		    } else
			ydir = ydir + 1
		newy = newy + ystep
		call gtr_writecursor (stream, newx, newy)

	    case 'L':
		# Step cursor right.
		if (speed == 0)
		    if (xdir > NSTEP) {
			xstep = MAX_STEP
			xdir = 1
		    } else if (xdir < 0) {
			xstep = max (MIN_STEP, xstep * SMALLER_STEP)
			xdir = 1
		    } else
			xdir = xdir + 1
		newx = newx + xstep
		call gtr_writecursor (stream, newx, newy)
	    
	    default:
		break
	    }

	    # We assume the cursor may have moved if the WC capability exists
	    # for this device.

	    if (ttygetb (gtr_gtty (stream), "WC")) {
		x = newx
		y = newy
	    }

	    # Print the cursor position.
	    if (ppos == YES)
		call grc_pcursor (stream, x, y, raster, rx, ry)
	}

	return (key)
end


# GRC_MAPKEY -- Map keystroke.  If the keystroke maps to a null value the key
# is not recognized as a cursor mode keystroke and we exit.  Note that if case
# sensitivity is disabled, KEYS comparisions must be made in upper case but
# only lower case is to be returned to the calling program.

int procedure grc_mapkey (rc, key, nukey)

pointer	rc			#I rcursor descriptor
int	key			#U raw key value
int	nukey			#O mapped key value

begin
	nukey = max(1, min(MAX_KEYS, key))
	if (RC_CASE(rc) == NO && IS_LOWER(nukey))
	    nukey = TO_UPPER(nukey)

	nukey = RC_KEYS(rc,nukey)
	if (nukey == NULL) {
	    # Not a cursor mode key.
	    if (RC_CASE(rc) == NO && IS_UPPER(nukey))
		key = TO_LOWER(key)
	} else if (IS_LOWER(nukey))
	    nukey = TO_UPPER(nukey)

	return (nukey)
end


# GRC_RESTORECURPOS -- Restore the cursor position in NDC coordinates
# regardless of the current workstation transformation.

procedure grc_restorecurpos (stream, x, y)

int	stream			# graphics stream
real	x, y			# new cursor position in NDC coords
real	sx, sy
include	"gtr.com"

begin
	call grc_ndctoscr (x, y, sx, sy)
	call gtr_writecursor (stream, sx, sy)
end


# GRC_READTTY -- Read from the terminal via the graphics kernel.  If the
# kernel already has message data buffered we merely return that data,
# otherwise we issue the prompt given and interactively read the data.

int procedure grc_readtty (stream, prompt, obuf, maxch)

int	stream			#I graphics stream
char	prompt[ARB]		#I prompt, if read is interactive
char	obuf[ARB]		#O output buffer
int	maxch			#I max chars out

bool	issue_prompt
int	nchars, index
int	stg_msglen(), stg_readtty()
int	stridxs(), strlen()

begin
	issue_prompt = (stg_msglen(STDIN) <= 0)
	if (issue_prompt)
	    call stg_putline (STDERR, prompt)

	nchars = stg_readtty (STDIN, obuf, maxch)
	index = stridxs ("\n", obuf)
	if (index > 0)
	    obuf[index] = EOS
	nchars = strlen (obuf)

	if (issue_prompt && nchars == 0)
	    call grc_message (stream, "\n\n")

	return (nchars)
end


# GRC_MESSAGE -- Write a message on the status line at the bottom of the
# screen.  If the string is not newline terminated the terminal is left in
# status line text mode.  To clear the status line and force the terminal
# back into graphics mode, output the string "\n\n".

procedure grc_message (stream, message)

int	stream			# graphics stream
char	message[ARB]		# message to be printed

begin
	call stg_putline (STDERR, message)
end


# GRC_PCURSOR -- Convert the cursor position in screen coordinates to world
# coordinates and print on the standard output.

procedure grc_pcursor (stream, sx, sy, raster, rx, ry)

int	stream			#I graphics stream
real	sx, sy			#I screen coords of cursor
int	raster			#I raster number
real	rx, ry			#I raster coords of cursor

int	wcs
real	xc, yc
pointer	sp, lbuf

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	call grc_scrtowcs (stream, sx, sy, raster, rx, ry, xc, yc, wcs)
	if (abs(xc) > 1 && abs(xc) < 10000 && abs(yc) > 1 && abs(yc) < 10000)
	    call sprintf (Memc[lbuf], SZ_LINE, "%10.3f %10.3f  \n")
	else
	    call sprintf (Memc[lbuf], SZ_LINE, "%12.7g %12.7g  \n")
	call pargr (xc)
	call pargr (yc)

	call stg_putline (STDERR, Memc[lbuf])
	call sfree (sp)
end

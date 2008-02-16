# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <chars.h>
include <ctype.h>
include "zdisplay.h"
include "iis.h"

define	NEXT_FRAME	'\006'
define	PREV_FRAME	'\022'
define	TOGGLE_MARK	'\015'

# IMDRCURO -- Read the logical image cursor from an already opened image
# display device (opened with IMDOPEN).  This is a high level cursor read,
# returning image pixel coordinates and relying upon the display server to use
# the keyboard or mouse to terminate the cursor read.  Nonblocking reads and
# frame buffer coordinates are available as options.  The cursor value is
# returned as an ascii string encoded as follows:
#
#       wx wy wcs key [strval]
#
# where WX,WY are the cursor coordinates in the coordinate system defined by
# WCS (= framenumber*100 + wcs, wcs=0 for frame buffer coordinates, wcs=1 for
# image pixel coordinates, the default), KEY is the keystroke used to terminate
# the cursor read, and STRVAL is the string value of the cursor, if key=':'
# (a colon command).  Nonprintable keys are returned as octal escapes.

procedure imdrcuro (tty, outstr, maxch, wcs, pause)

pointer	tty			#I graphcap descriptor for device
char    outstr[maxch]           #O formatted output cursor value
int     maxch                   #I max chars out
int     wcs                     #I desired wcs: 0=framecoords, 1=imagecoords
int     pause                   #I blocking cursor read? (YES|NO)

short	cursor[3]
char	key, str[1]
short	split[LEN_SPLIT]
pointer	sp, strval, imcurval
real	a, b, c, d, tx, ty, wx, wy
int     status, frame, tid, z, n, keystat, sx, sy, ip, chan, i

bool	mark_cursor
data	mark_cursor /false/

bool	ttygetb()
int	rdukey(), ttygeti(), cctoc(), iisflu(), imd_getwcs()
define	again_ 91
include "iis.com"

begin
	call smark (sp)
	call salloc (strval, SZ_LINE, TY_CHAR)
	call salloc (imcurval, SZB_IMCURVAL, TY_CHAR)

	if (ttygetb (tty, "LC")) {
	    # Logical image cursor read; the display server supports the
	    # logical image cursor read as an atomic operation, via the
	    # logical subunit IMCURSOR (an IRAF special extension to the
	    # regular IIS datastream protocol).

	    if (pause == NO)
		tid = IREAD + SAMPLE
	    else
		tid = IREAD

	    call iishdr (tid, SZB_IMCURVAL, COMMAND+IMCURSOR, 0,0, wcs, 0)

	    call iisio (Memc[imcurval], SZB_IMCURVAL, status)
	    if (status <= 0)
		call strcpy ("EOF\n", outstr, maxch)
	    else
		call strupk (Memc[imcurval], outstr, maxch)

	} else {
	    # IIS compatible cursor read.  Implement the logical cursor read
	    # using only the primitive IIS cursor functions and the terminal
	    # driver, accessing the WCS file directly to get the coordinate
	    # transformation from IIS device coords to image pixel coords.

	    # Pick up the frame size and configuration number.
	    iis_xdim   = ttygeti (tty, "xr")
	    iis_ydim   = ttygeti (tty, "yr")
	    iis_config = ttygeti (tty, "cn")
again_
	    if (pause == YES) {
		# Enable cursor blink to indicate cursor read in progress.
		call iishdr (IWRITE+VRETRACE,1,COMMAND+CURSOR, ADVXONTC, 0,0,0)
		cursor[1] = 57B
		call iisio (cursor, SZ_SHORT * SZB_CHAR, status)

		# Wait for the user to type a key on the keyboard.  The value
		# is returned as a newline delimited string.

		keystat = rdukey (Memc[strval], SZ_LINE)

	    } else {
		Memc[strval] = '\n'
		Memc[strval+1] = EOS
		keystat = 1
	    }

	    # Sample the cursor position.
	    call iisrcr (status, sx, sy)
	    sx = sx / MCXSCALE
	    sy = sy / MCYSCALE

	    # Determine which frame was being displayed.
	    call iishdr (IREAD, LEN_SPLIT, COMMAND+LUT, ADVXONTC, 0,0,0)
	    call iisio (split, LEN_SPLIT * SZB_CHAR, status)

	    z = split[1]
	    if (z == 0)
		z = 1
	    for (n=1;  and(z,1) == 0;  z = z / 2)
		n = n + 1
	    frame = max(1, min(4, n))
	    chan = FRTOCHAN(frame)

	    if (pause == YES) {
		# Turn off cursor blink.
		call iishdr (IWRITE+VRETRACE,1,COMMAND+CURSOR, ADVXONTC, 0,0,0)
		cursor[1] = 47B
		call iisio (cursor, SZ_SHORT * SZB_CHAR, status)
	    }

	    # Decode the trigger keystroke.
	    ip = 1
	    if (cctoc (Memc[strval], ip, key) <= 0)
		key = 0

	    # Check for the builtin pseudo "cursor mode" commands.
	    switch (key) {
	    case NEXT_FRAME:
		# Display the next frame in sequence.
		frame = frame + 1
		if (frame > 4)
		    frame = 1
		chan = IIS_CHAN * DEVCODE + frame
		call iisrgb (chan, chan, chan)
		goto again_
	    case PREV_FRAME:
		# Display the previous frame.
		frame = frame - 1
		if (frame <= 0)
		    frame = 1
		chan = IIS_CHAN * DEVCODE + frame
		call iisrgb (chan, chan, chan)
		goto again_
	    case TOGGLE_MARK:
		# Toggle the mark cursor enable.
		mark_cursor = !mark_cursor
		goto again_
	    }

	    # Mark the cursor position by editing the frame buffer.
	    if (mark_cursor && keystat > 1 && key != '\004' && key != '\032') {
		do i = 1, 3
		    cursor[i] = 1
		call achtsb (cursor, cursor, 3)
		
		call iishdr (IWRITE+BYPASSIFM+PACKED+VRETRACE, 3, REFRESH,
		    or(sx-1,ADVXONTC), or(sy-1,ADVYONXOV),
		    iisflu(chan), ALLBITPL)
		call iisio (cursor, 3, status)

		call iishdr (IWRITE+BYPASSIFM+PACKED+VRETRACE, 3, REFRESH,
		    or(sx-1,ADVXONTC), or(sy,ADVYONXOV),
		    iisflu(chan), ALLBITPL)
		call iisio (cursor, 3, status)

		call iishdr (IWRITE+BYPASSIFM+PACKED+VRETRACE, 3, REFRESH,
		    or(sx-1,ADVXONTC), or(sy+1,ADVYONXOV),
		    iisflu(chan), ALLBITPL)
		call iisio (cursor, 3, status)
	    }

	    # Perform the transformation to image pixel coordinates.
	    if (wcs != 0) {
		if (imd_getwcs (frame,NO, str,0,str,0, a,b,c,d,tx,ty) == ERR) {
		    call eprintf ("Warning: cannot retrieve WCS for frame %d\n")
			call pargi (frame)
		}
		if (abs(a) > .001)
		    wx = sx * a + tx
		if (abs(d) > .001)
		    wy = sy * d + ty
	    } else {
		wx = sx
		wy = sy
	    }

	    # Format the output cursor value string.
	    if (keystat == EOF)
		call strcpy ("EOF\n", outstr, maxch)
	    else {
		call sprintf (outstr, maxch, "%.6g %.6g %d %s")
		    call pargr (wx)
		    call pargr (wy)
		    call pargi (frame * 100 + wcs)
		    call pargstr (Memc[strval])
	    }
	}

	call sfree (sp)
end

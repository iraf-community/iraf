# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# PAN -- pan some or all of the frames

procedure pan()

char	token[SZ_LINE]
int	tok
short	frames[IDS_MAXIMPL+2]		# frames, graphics, EOD

include "cv.com"

begin
	frames[1] = IDS_EOD		# default all frames
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (token[1] == 'f') {
	    call cv_frame (token[2], frames)
	    if (frames[1] == ERR)
		return
	} else if (tok == TOK_NUMBER) {
	    call cv_frame (token[1], frames)
	    if (frames[1] == ERR)
		return
	} else {
	    call eprintf ("Unexpected input: %s\n")
		call pargstr (token)
	    return
	}

	call pansub (frames)
end


# PANSUB -- Pan subroutine, handles code common to pan and zoom

procedure pansub (frames)

short	frames[ARB]			# frames to pan

int	button
int	cnum, cv_rdbut()
real	x,y, xc, yc
real	oldx, oldy

include	"cv.com"

begin
	button = cv_rdbut()		# clear buttons by reading them
	call eprintf ("Press any button when done\n")

	# Where is cursor now?

	call cv_rcraw (xc,yc)

	# Calculate NDC screen center and cursor number.
	# x,y are NDC, but always < 1.0  The transformation applied here
	# insures that the correct pixel is calculated by the kernel
	# after passing x,y through the gio cursor routines.
	x = real(cv_xcen - 1) * cv_xcon / GKI_MAXNDC
	y = real(cv_ycen - 1) * cv_ycon / GKI_MAXNDC
	cnum = frames[1]
	if (cnum == IDS_EOD)
	    cnum = 0
	call cv_scraw (x, y)	# put cursor at screen center

	# Determine NDC there for frame of interest
	call cv_rcur (cnum, x, y)

	# Restore cursor
	call cv_scraw (xc, yc)

	repeat {
	    oldx = xc
	    oldy = yc
	    repeat {
	        call cv_rcraw (xc, yc)
	        button = cv_rdbut()
	    } until ( (xc != oldx) || (yc != oldy) || (button > 0))
	    # Determine change and reflect it about current screen
	    # center so image moves in direction cursor moves.
	    x = x - (xc - oldx)
	    y = y - (yc - oldy)
	    if (x > 1.0)
		x = x - 1.0
	    else if (x < 0)
		x = x + 1.0
	    if (y > 1.0)
		y = y - 1.0
	    else if (y < 0)
		y = y + 1.0
	    call cvpan (frames, x, y)
	} until (button > 0)
end

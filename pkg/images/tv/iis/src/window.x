# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# WINDOW -- window the display.

procedure window()

char	token[SZ_LINE]
int	tok, cnum
short	frames[IDS_MAXIMPL+2]			# frames, graphics, EOD
short	colors[IDS_MAXGCOLOR]
real	x, y
real	xold, yold
int	device, button, cv_rdbut()
short	wdata[16]
int	n, first, last
real	istart, iend, slope

include "cv.com"

begin
	# Find out if want to change output tables
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (( tok == TOK_IDENTIFIER) && (token[1] == 'o')) {
	        device = IDS_OUTPUT_LUT
		slope = 4.0			# Device dependent !!
	} else {
	    device = IDS_FRAME_LUT
	    slope = 1.0
	    # reset input pointers; same as having pushed back token
	    call reset_scan
	    call gargtok (tok, token, SZ_LINE)
	}

	# Default to all frames, all colors
	frames[1] = IDS_EOD
	colors[1] = IDS_EOD

	# which frames to window

	repeat {
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	    if (tok == TOK_IDENTIFIER) {
	        if (token[1] == 'f') {
	            call cv_frame (token[2], frames)
	            if (frames[1] == ERR)
		        return
	        } else if (token[1] == 'c') {
		    call cv_color (token[2], colors)
		    if (colors[1] == ERR)
		        return
	        } else {
		    call eprintf ("Unknown window argument: %s\n")
			call pargstr (token)
		    return
		}
	    } else if (tok == TOK_NUMBER) {
		call cv_frame (token[1], frames)
		if (frames[1] == ERR)
		    return
	    } else if (tok != TOK_NEWLINE) {
		call eprintf ("Unexpected window input: %s\n")
		    call pargstr (token)
		return
	    }
	} until ( tok == TOK_NEWLINE)

	# rememeber current cursor postion

	cnum = 0
	call cv_rcur (cnum, xold, yold)

	# Now set up loop to window display;  we need to read back
	# display but cannot, so for now, use "common" variables
	# If first time, use defaults.

	if (cv_xwinc == -1) {
	    if (slope == 1.0) {
	        cv_xwinc = 0.25
	        cv_ywinc = .75
	    } else {
		cv_xwinc = .0625
		cv_ywinc = .9375
	    }
	}
	call cv_scraw (cv_xwinc, cv_ywinc)

	button = cv_rdbut()		# clear buttons by reading them
	call eprintf ("Press any button when done\n")

	# The mapping equation is  table value = 0.25 + y * (i-x)
	# where i runs from 0 to 1.0, x ranges from 0. to 1.0 and y
	# from 0 to large.

	repeat {
	    call cv_rcraw (cv_xwinc, cv_ywinc)
	    x = cv_xwinc
	    y = (cv_ywinc - 0.5) * 4
	    # Keep y from equalling 2 or -2 :
	    if (y >= 2.)
		y = 1.99
	    else if ( y <= -2.0)
		y = -1.99
	    if (y > 1.)
		y = 1. / (2. - y)
	    else if (y < -1.)
		y = -1. / (2. + y)

	    if ( y == 0.0) {
		iend = 1.0
		istart = 0.0
		first = 0
		last = GKI_MAXNDC
	    } else if ( y > 0.) {
		istart = x - 0.25/y
		iend = 1.0/y + istart
		first = 0
		last = GKI_MAXNDC
	    } else {
		iend = x - 0.25/y
		istart = 1.0/y + iend
		first = GKI_MAXNDC
		last = 0
	    }
	    if (istart < 0.)
	        istart = 0.
	    if (iend > 1.0)
		iend = 1.0
	    if (istart > 1.0)
		istart = 1.0
	    if (iend < istart)
		iend = istart
	    wdata[1] = 0
	    if ( istart > 0.) {
		    wdata[2] = first
		    wdata[3] = istart * GKI_MAXNDC
		    wdata[4] = first
		    n = 5
	    } else {
		wdata[2] = (0.25 -x*y) * GKI_MAXNDC
		n = 3
	    }
	    wdata[n] = iend * GKI_MAXNDC
	    if ( iend < 1.0) {
		# In this case, we reach max/min y value before end of table, so
		# extend it horizontally to end
		wdata[n+1] = last
	    	wdata[n+2] = GKI_MAXNDC
		wdata[n+3] = last
		n = n + 3
	    } else {
	        wdata[n+1] = (0.25 + y * (1.0 - x)) * GKI_MAXNDC
	        n = n + 1
	    }
	    call cvwlut (device, frames, colors, wdata, n)
	    button = cv_rdbut()
	} until (button > 0)

	# Restore old cursor position
	call cv_rcur (cnum, xold, yold)

	# Tell the user what final mapping was
	call printf ("window: from (%5.3f,%5.3f) to (%5.3f,%5.3f)\n")
	    call pargr (istart)
	    if (istart > 0.)
	        call pargr (real(first)/GKI_MAXNDC)
	    else
	        call pargr (real(wdata[2])/GKI_MAXNDC)
	    call pargr (iend)
	    if (iend < 1.0)
	        call pargr (real(last)/GKI_MAXNDC)
	    else
	        call pargr (real(wdata[n])/GKI_MAXNDC)

end

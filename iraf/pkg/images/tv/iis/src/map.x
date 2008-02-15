# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# MAP -- set fixed or variable LUT mapping

procedure map(command)

char	command[ARB]

char	token[SZ_LINE]
int	tok
short	frames[IDS_MAXIMPL+2]			# frames, graphics, EOD
short	colors[IDS_MAXGCOLOR]
int	device
short	pcolor[2]
real	limit
long	seed
real	urand(), xfactor
int	ctoi()
int	i, ip, iseed, level, nchar
bool	triangle
pointer	sp, rdata, gdata, bdata, rp, gp, bp

include "cv.com"

begin
	# Find out if want to change output tables
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (( tok == TOK_IDENTIFIER) && (token[1] == 'o' )) {
	        device = IDS_OUTPUT_LUT
	} else {
	    device = IDS_FRAME_LUT
	    # reset input pointers; same as having pushed back token
	    call reset_scan
	    call gargtok (tok, token, SZ_LINE)
	}

	# Default to all frames, all colors
	frames[1] = IDS_EOD
	colors[1] = IDS_EOD
	triangle = true		# default to simple three function type
	seed = -1
	level = 8

	# which frames to change, colors, etc

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
		} else if (token[1] == 'r') {	# (random) level count
		    ip = 2
		    nchar = ctoi (token, ip, level)
		    if (nchar <= 0) {
			call eprintf ("Incorrect random count: %s\n")
			    call pargstr (token[2])
			return
		    }
		    if (level < 4)
			level = 4
		    else if (level > 128)
			level = 128
		    triangle = false
		} else if (token[1] == 's') {	# seed
		    ip = 2
		    nchar = ctoi (token, ip, iseed)
		    if (nchar <= 0) {
			call eprintf ("Incorrect seed: %s\n")
			    call pargstr (token[2])
			return
		    }
		    seed = iseed
		    triangle = false
	        } else {
		    call eprintf ("Unknown map argument: %s\n")
			call pargstr (token)
		    return
		}
	    } else if (tok != TOK_NEWLINE) {
		call eprintf ("Unexpected map input: %s\n")
		    call pargstr (token)
		return
	    }
	} until ( tok == TOK_NEWLINE)

	pcolor[2] = IDS_EOD
	# Sorry, but we "know" that ofm shouldn't go beyond first
	# 256 for common NOAO use.
	if ( device == IDS_FRAME_LUT)
	    limit = 1.0
	else
	    limit = 0.25

	# Build the three functions and load them.
	# First, expand colors if using all

	if (colors[1] == IDS_EOD) {
	    colors[1] = IDS_RED
	    colors[2] = IDS_GREEN
	    colors[3] = IDS_BLUE
	    colors[4] = IDS_EOD
	}

	# if standard pseudocolor, let kodak do it

	if (triangle) {
	    call kodak (device, frames, colors, limit)
	    return
	}

	# Not standard pseudo color -- do random one
	# First, set up arrays

	call smark (sp)
	call salloc (rdata, level*4, TY_SHORT)
	call salloc (gdata, level*4, TY_SHORT)
	call salloc (bdata, level*4, TY_SHORT)

	if (seed == -1)
	    seed = level

	call aclrs (Mems[rdata], level*4)
	call aclrs (Mems[gdata], level*4)
	call aclrs (Mems[bdata], level*4)

	xfactor = real(GKI_MAXNDC)/level * limit

	# set first data points to zero (0,0) to (1/level,0)
	Mems[rdata+2] = xfactor
	Mems[gdata+2] = xfactor
	Mems[bdata+2] = xfactor
	# Set last segment to white ((level-1)/level,1.0) to (1.0,1.0)
	Mems[rdata+level*4-4] = real(level-1) * xfactor
	Mems[gdata+level*4-4] = real(level-1) * xfactor
	Mems[bdata+level*4-4] = real(level-1) * xfactor
	Mems[rdata+level*4-3] = GKI_MAXNDC
	Mems[gdata+level*4-3] = GKI_MAXNDC
	Mems[bdata+level*4-3] = GKI_MAXNDC
	Mems[rdata+level*4-2] = GKI_MAXNDC
	Mems[gdata+level*4-2] = GKI_MAXNDC
	Mems[bdata+level*4-2] = GKI_MAXNDC
	Mems[rdata+level*4-1] = GKI_MAXNDC
	Mems[gdata+level*4-1] = GKI_MAXNDC
	Mems[bdata+level*4-1] = GKI_MAXNDC

	# Do the intermediate ones
	do i=2, level-1 {
	    rp = rdata + (i-1)*4
	    gp = gdata + (i-1)*4
	    bp = bdata + (i-1)*4
	    Mems[rp] = real(i-1) * xfactor
	    Mems[gp] = real(i-1) * xfactor
	    Mems[bp] = real(i-1) * xfactor
	    Mems[rp+1] = urand(seed) * GKI_MAXNDC
	    Mems[gp+1] = urand(seed) * GKI_MAXNDC
	    Mems[bp+1] = urand(seed) * GKI_MAXNDC
	    Mems[rp+2] = real(i) * xfactor
	    Mems[gp+2] = real(i) * xfactor
	    Mems[bp+2] = real(i) * xfactor
	    Mems[rp+3] = Mems[rp+1]
	    Mems[gp+3] = Mems[gp+1]
	    Mems[bp+3] = Mems[bp+1]
	}

	# If color requested, do it
	for ( i = 1; colors[i] != IDS_EOD; i = i + 1 ) {
	    pcolor[1] = colors[i]
	    switch (colors[i]) {
		case IDS_RED:
	    	    call cvwlut (device, frames, pcolor, Mems[rdata], level*4)

		case IDS_GREEN:
	    	    call cvwlut (device, frames, pcolor, Mems[gdata], level*4)

		case IDS_BLUE:
	    	    call cvwlut (device, frames, pcolor, Mems[bdata], level*4)
	    }
	}

	call sfree (sp)
end

# KODAK -- provides three variable width and variable center triangular
# color mapping functions.

procedure kodak (device, frames, colors, limit)

int	device				# IDS_FRAME_LUT or IDS_OUTPUT_LUT
short	frames[ARB]			# frames to change
short	colors[ARB]			# colors to affect
real	limit				# factor to apply to limit x range

short	wdata[20], pcolor[2]
real	center, width
int	n, ksub(), button, i
int	cv_rdbut(), cv_wtbut()

begin
	pcolor[2] = IDS_EOD
	for (i = 1; colors[i] != IDS_EOD; i = i + 1) {
	    pcolor[1] = colors[i]
	    switch (colors[i]) {
		case IDS_RED:
		    n = ksub (1.0, 0.5, wdata, limit)

		case IDS_GREEN:
		    n = ksub (0.5, 0.5, wdata, limit)

		case IDS_BLUE:
		    n = ksub (0.0, 0.5, wdata, limit)
	    }

	    call cvwlut (device, frames, pcolor, wdata, n)
	}

	button = cv_rdbut()		# clear buttons
	repeat {
	    call eprintf ("Press A, B, C for red, green, blue; D to exit\n")
	    button = cv_wtbut()
	    if (button == 4)
		break
	    switch (button) {
		case 1:
	    	    pcolor[1] = IDS_RED

		case 2:
	    	    pcolor[1] = IDS_GREEN

		case 3:
	    	    pcolor[1] = IDS_BLUE
	    }
	    
	    # Loop, reading cursor and modifying the display for the
	    # selected color.

	    repeat {
		call cv_rcraw(center, width)
		width = width * 2.		# flatten it
		n = ksub (center, width, wdata, limit)
		call cvwlut (device, frames, pcolor, wdata, n)
		button = cv_rdbut()
	    } until (button != 0)
	}
end

# KSUB -- determines data points for a triangular mapping function
# Returns number of points in data array.

int procedure ksub (center, width, data, limit)

real	center, width, limit
short	data[ARB]

int	n
real	xs, xe, ys, ye, xscale

include "cv.com"

begin
	n = 0
	xscale = GKI_MAXNDC * limit
	if (width < (1.0/cv_yres))
	    width = 1.0/cv_yres
	
	if (center > 0.) {
	    xs = center - width
	    if (xs < 0.)
		xs = 0.
	    else if (xs > 0.) {
		data[1] = 0.
		data[2] = 0.
		n = n + 2
	    }
	    ys = (xs - center)/width + 1.0
	    data[n+1] = xs * xscale
	    data[n+2] = ys * GKI_MAXNDC
	    data[n+3] = center * xscale
	    data[n+4] = GKI_MAXNDC
	    n = n + 4
	}

	if (center < 1.0) {
	    xe = width + center
	    if (xe > 1.0)
		xe = 1.0
	    ye = (center - xe)/width + 1.0
	    data[n+1] = center * xscale
	    data[n+2] = GKI_MAXNDC
	    data[n+3] = xe * xscale
	    data[n+4] = ye * GKI_MAXNDC
	    n = n + 4
	    if (xe < 1.0) {
		data[n+1] = xscale
		data[n+2] = 0
		n = n + 2
	    }
	}

	# Extend last value to end
	if (limit != 1.0) {
	    data[n+1] = GKI_MAXNDC
	    data[n+2] = data[n]
	    n = n + 2
	}

	return (n)
end

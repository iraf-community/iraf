# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>

define	SZ_LABEL	10
define	SZ_FMT		20

# DRAW_PERIMETER -- draw and annotate the axes drawn around the perimeter
# of the image pixels.  The viewport and window have been set by 
# the calling procedure.  Plotting is done in window coordinates.
# This procedure is called by both crtpict and the ncar plotting routines
# contour and hafton.

procedure draw_perimeter (gp)

pointer	gp			# Graphics descriptor
real	xs, xe, ys, ye		# WCS coordinates of pixel window

int	i, first_col, last_col, first_tick, last_tick, bias
int	nchar, dummy, first_row, last_row, cnt_step, cnt_label
char	label[SZ_LABEL], fmt1[SZ_FMT], fmt2[SZ_FMT], fmt3[SZ_FMT], fmt4[SZ_FMT]
real	dist, kk, col, row, dx, dy, sz_char, cw, xsz, label_pos
real	xdist, ydist, xspace, yspace
bool	ggetb()
int	itoc()
real 	ggetr()
real	k[3]
data 	k/1.0,2.0,3.0/
errchk	ggwind, gseti, gctran, gline, gtext, itoc

begin
	# First, get window coordinates and turn off clipping
	call ggwind (gp, xs, xe, ys, ye)
	call gseti (gp, G_CLIP, NO)

	# A readable character width seems to be about 1.mm.  A readable
	# perimeter seperation seems to be about .80mm.  If the physical
	# size of the output device is contained in the graphcap file, the
	# NDC sizes of these measurements can be determined.  If not, 
	# the separation between perimeter axes equals one quarter character
	# width or one quarter percent of frame, which ever is larger, and 
	# the character size is set to 0.40.

	cw = max (ggetr (gp, "cw"), 0.01)
	if (ggetb (gp, "xs")) {
	    xsz = ggetr (gp, "xs")
	    dist = .80 / (xsz * 1000.)
	    sz_char = dist / cw
	} else {
	    # Get character width and calculate perimeter separation.
	    dist = cw * 0.25
	    sz_char = 0.40
	}

	# Convert distance to user coordinates
	call ggscale (gp, xs, ys, dx, dy)
	xdist = dist * dx
	ydist = dist * dy

	# Generate four possible format strings for gtext
	call sprintf (fmt1, SZ_LINE, "h=c;v=t;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt2, SZ_LINE, "h=c;v=b;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt3, SZ_LINE, "h=r;v=c;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt4, SZ_LINE, "h=l;v=c;s=%.2f")
	    call pargr (sz_char)

	# Draw inner and outer perimeter
	kk = k[1]
	do i = 1, 2 {
	    xspace = kk * xdist
	    yspace = kk * ydist
	    call gline (gp, xs - xspace, ys - yspace, xe + xspace, ys - yspace)
	    call gline (gp, xe + xspace, ys - yspace, xe + xspace, ye + yspace)
	    call gline (gp, xe + xspace, ye + yspace, xs - xspace, ye + yspace)
	    call gline (gp, xs - xspace, ye + yspace, xs - xspace, ys - yspace)
	    kk = k[2]
	}

	# Now draw x axis tick marks, along both the bottom and top of
	# the picture.  First find the endpoint integer pixels.

	first_col = int (xs)
	last_col = int (xe)

	# Determine increments of ticks and tick labels for x axis
	cnt_step  = 1
	cnt_label = 10
	if (last_col - first_col > 256) {
	    cnt_step = 10
	    cnt_label = 100
	} else if (last_col - first_col < 26) {
	    cnt_step = 1
	    cnt_label = 1
	}

	first_tick = first_col
	bias = mod (first_tick, cnt_step)
	last_tick = last_col + bias

	do i = first_tick, last_tick, cnt_step {
	    col = real (i - bias)
	    call gline (gp, col, ys - k[1] * ydist, col, ys - k[2] * ydist)
	    call gline (gp, col, ye + k[1] * ydist, col, ye + k[2] * ydist)

	    if (mod ((i - bias), cnt_label)  == 0) {
		# Label tick mark; calculate number of characters needed
		nchar = 3
		if (int (col) == 0)
		    nchar = 1
		if (int (col) >= 1000)
		    nchar = 4

		dummy = itoc (int(col), label, nchar)

		# Position label slightly below outer perimeter.  Seperation
		# is twenty percent of a character width, in WCS.
		label_pos = ys - (k[2] * ydist + (cw * 0.20 * dy))
		call gtext (gp, col, label_pos, label, fmt1)

		# Position label slightly above outer perimeter
		label_pos = ye + (k[2] * ydist + (cw * 0.20 * dy))
		call gtext (gp, col, label_pos, label, fmt2)
	    }
	}

	# Label the y axis tick marks along the left and right sides of the
	# picture.  First find the integer pixel endpoints.
	
	first_row = int (ys)
	last_row = int (ye)

	# Determine increments of ticks and tick labels for y axis
	cnt_step  = 1
	cnt_label = 10
	if (last_row - first_row > 256) {
	    cnt_step = 10
	    cnt_label = 100
	} else if (last_row - first_row < 26) {
	    cnt_step = 1
	    cnt_label = 1
	}

	first_tick = first_row 
	bias = mod (first_tick, cnt_step)
	last_tick = last_row + bias

	do i = first_tick, last_tick, cnt_step {
	    row = real (i - bias)
	    call gline (gp, xs - k[1] * xdist, row, xs - k[2] * xdist, row)
	    call gline (gp, xe + k[1] * xdist, row, xe + k[2] * xdist, row)

	    if (mod ((i - bias), cnt_label) == 0) {
		# Label tick mark; calculate number of characters needed
		nchar = 3
		if (int (row) == 0)
		    nchar = 1
		else if (int (row) >= 1000)
		    nchar = 4
		
	        dummy = itoc (int(row), label, nchar)

		# Position label slightly to the left of outer perimeter.
		# Separation twenty percent of a character width, in WCS.
		label_pos = xs - (k[2] * xdist + (cw * 0.20 * dx))
	        call gtext (gp, label_pos, row, label, fmt3)

		# Position label slightly to the right of outer perimeter
		label_pos = xe + (k[2] * xdist + (cw * 0.20 * dx))
	        call gtext (gp, label_pos, row, label, fmt4)
	    }
	}
end

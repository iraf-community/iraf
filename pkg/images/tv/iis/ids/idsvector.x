# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <fset.h>
include	"../lib/ids.h"

define MAXC	10000		# just a largish int here

# IDS_VECTOR -- Plot a line in the current plane; the starting coordinates
# are in ids.com: i_pt_x, i_pt_y.  The end points are the arguments
# to vector.
# the code is Bresenham's algorithm, as taken from the line drawing
# routine in Forth-11 image display code.

procedure ids_vector (ax,ay)

short	ax,ay			# vector end coordinates, GKI

short	x,y
short	xe ,ye			# end coordinates, device
short	dx,dy,dd
short	xi,yi, xid,yid		# increments
short	total, e		# total change and error
int	bufsize			# file i/o buffersize
int	fstati()
int	count, cmax

include	"../lib/ids.com"

begin
	x = ax
	y = ay

	bufsize = fstati(i_out, F_BUFSIZE)

	# convert x,y to device coords.
	xe = real(x) * i_xres /(GKI_MAXNDC+1)
	ye = real(y) * i_yres /(GKI_MAXNDC+1)

	# determine delta x and y, and x/y increments

	dx = xe - i_pt_x
	dy = ye - i_pt_y

	# set movement increments, take absolute value of dx, dy
	if ( dy >= 0 )
	    yi = 1
	else {
	    yi = -1
	    dy = -dy
	}
	if ( dx >= 0 )
	    xi = 1
	else {
	    xi = -1
	    dx = -dx
	}

	# set diagonal movement increments
	xid = xi
	yid = yi

	# if, for instance, pos. slope less than 45 degrees, most movement
	# is in x, so then set (the ususal) y increment to zero
	if ( dy >= dx )
	    xi = 0
	else
	    yi = 0

	# Set up for buffer of one, and let code find best buffering
	cmax = 0
	call fseti(i_out, F_BUFSIZE, 1)
	count = 0

	# Plot the first point
	call ids_rpoint (0, 0)

	# Is there anything to do?  determine total increments to plot; if
	# zero, quit
	total = dx + dy
	if ( total == 0 ) {
	    call fseti (i_out, F_BUFSIZE, bufsize)
	    return
	}
	
	# set error to zero, determine difference in x,y change.
	e = 0
	dd = dy - dx
	if ( dd >= 0 ) {
	    dd = -dd
	    dy = dx
	}

	# plot the line
	repeat {
	    dx = dd + e
	    if ( (dy + e + dx) >= 0 ) {
	    # diagonal plot, accounts for two units of increment
		if ( count > cmax ) {
		# leaving current (x) line, so determine how many points
		# have plotted on line and use this (maximum) as line
		# buffering size
		    call fseti(i_out, F_BUFSIZE, count)
		    cmax = count
		    count = 0
		}
		call ids_rpoint ( xid, yid )
		total = total - 2
		e = dx
	    } else {
	        # move in x (or y) only; for the small positive slope line, 
	        # real line will move up and finally over line being plotted,
	        # hence e increases.
		call ids_rpoint ( xi, yi )
		total = total - 1
		e = e + dy
	        count = count + 1
	    }
	} until ( total <= 0 )
	# restore original buffer size
	call fseti(i_out, F_BUFSIZE, bufsize)
end

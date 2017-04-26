# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <fset.h>
include "iis.h"
include "../lib/ids.h"

# DO_SNAP -- Return a line of the active image display, as seen
# by the viewer.

procedure do_snap (buf, nchar, xpos, ypos)

short	buf[ARB]			# buffer to read into
int	nchar				# how many to read
int	xpos, ypos			# and from where

int	y, yindex, xs, xe
int	line, previous
int	i,j
int	yedge
int	zm, count
bool	first

include "../lib/ids.com"
include "iis.com"
include "zsnap.com"

begin
	# Check if read is for one line only

	if (nchar > IIS_XDIM) {
	    call eprintf("ZSNAP -- too many pixels (%d) requested.\n")
	        call pargi (nchar)
	    call aclrs (buf, nchar)
	    return
	}

	# Determine x and y coordinates on screen.

	y = IIS_YDIM - 1 - ypos
	xs = xpos
	xe = xs + nchar - 1
	count = nchar

	# See if we are dealing with (a part of only) one line

	if (xe >= IIS_XDIM) {
	    call eprintf("ZSNAP -- line overlap error (xend is %d).\n")
		call pargi (xe)
	    call aclrs (buf, nchar)
	    return
	}

	# Determine whether above or below split point.

	if (y < ysplit)
	    yindex = 1
	else
	    yindex = 2

	# Clear accumulators

	do j = sn_start, sn_end
	    call aclrs (Mems[result[j]], IIS_XDIM)

	# Fetch and massage data for each active frame

	first = true
	previous = -1		# a bit of safety if no frames on
	do i = 1, i_maxframes {
	    if (on[i]) {
		# If frame not active in any color for this half of screen,
		# ignore it
		if (sn_start != sn_end) {
		    if ((left[BLU, yindex, i] == -1) &&
		        (left[GR , yindex, i] == -1) &&
		        (left[RD , yindex, i] == -1) )
		         next
		} else if (left[sn_start, yindex, i] == -1)
		    next

		zm = zoom[i]
		iplane = 377B			# all bit planes
		iframe = 2**(i-1)

		# y edge of frame (top) [ see zcursor_set for more information]
		yedge = IIS_YCEN - yscroll[i] + IIS_YCEN_INV - IIS_YCEN_INV/zm
		if (yedge < 0)
		    yedge = yedge + IIS_YDIM

		# Desired y (screen) coordinate
		line = yedge + y/zm
		if (line >= IIS_YDIM)
	    	    line = line - IIS_YDIM
	        # If have done this line before, just return the same answer

		if (first) {
		    if (line == prev_y) {
	    	        call amovs (Mems[answer], buf, nchar)
	    	        return
		    }
		    previous = line
		    first = false
		}

		# Turn line into file position.
		line = IIS_YDIM - 1 - line
		if (multi_frame)
		    call fseti (sn_fd, F_CANCEL, OK)
		call zseek (sn_fd, xs, line)
		call read (sn_fd, Mems[input], count)
		call zmassage (zm, xscroll[i], yindex, i, xs, xe)
	    }
	}

	# Apply scaling

	do j = sn_start, sn_end {
	    # Note...xs, xe are zero-based indices
	    if ( offset[j] != 0)
	        call aaddks (Mems[result[j]+xs], offset[j],
		             Mems[result[j]+xs], count)
	    if ( range[j] != 1)
	        call adivks (Mems[result[j]+xs], range[j],
	   		     Mems[result[j]+xs], count)
	    call aluts (Mems[result[j]+xs], Mems[result[j]+xs], count,
			Mems[ofmp[j]])
	}

	# Or in the graphics ... use of "select" (asel) depends on design
	# decision in zdisplay_g.x

	if (gr_in_use) {
	    iframe = GRCHAN
	    iplane = 177B		# ignore cursor plane
	    zm = zoom[GRCHNUM]

	    yedge = IIS_YCEN - yscroll[GRCHNUM] + IIS_YCEN_INV - IIS_YCEN_INV/zm
	    if (yedge < 0)
	        yedge = yedge + IIS_YDIM

	    line = yedge + y/zm
	    if (line >= IIS_YDIM)
	        line = line - IIS_YDIM
	    line = IIS_YDIM - 1 - line

	    if (multi_frame)
		call fseti (sn_fd, F_CANCEL, OK)

	    call zseek (sn_fd, xs, line)
	    call read (sn_fd, Mems[input], count)
	    call zmassage (zm, xscroll[GRCHNUM], yindex, GRCHNUM, xs, xe)

	    do j = sn_start, sn_end {
	        call aluts (Mems[input+xs], Mems[zs], count, Mems[grp[j]])

	        # Build boolean which says if have graphics on
	        call abneks (Mems[zs], short(0), Memi[grbit_on], count) 

	        # With INSERT on: replace data with graphics.
	        call asels (Mems[zs], Mems[result[j]+xs], Mems[result[j]+xs],
		    Memi[grbit_on], count)
	    }
	}

	# The answer is:

	if (sn_start != sn_end) {
	    call aadds (Mems[result[BLU]], Mems[result[GR]],
	        Mems[answer], IIS_XDIM)
	    call aadds (Mems[answer], Mems[result[RD]], Mems[answer], IIS_XDIM)
	    call adivks (Mems[answer], short(3), Mems[answer], IIS_XDIM)
	} else {
	    # Put in "answer" so repeated lines are in known location
	    call amovs (Mems[result[sn_start]], Mems[answer], nchar)
	}

	# Set the previous line and return the answer

	prev_y = previous
	call amovs (Mems[answer], buf, nchar)
end


# ZMASSAGE --- do all the boring massaging of the data: zoom, scroll, look
# up tables.

procedure zmassage (zm, xscr, yi, i, xstart, xend)

int	zm			# zoom factor
short	xscr			# x scroll
int	yi			# y-index
int	i			# frame index
int	xstart, xend		# indices for line start and end

int	lb, count		# left bound, count of number of items
int	j, x1, x2, itemp
include	"zsnap.com"

begin
	if ( (xscr != IIS_XCEN) || (zm != 1)) {
	    if (xscr == IIS_XCEN)
		# Scrolling not needed
		call amovs (Mems[input], Mems[zs], IIS_XDIM)
	    else {
		# Scroll the data
		lb = xscr - IIS_XCEN
		if ( lb < 0 )
		    lb = lb + IIS_XDIM
		count = IIS_XDIM - lb
		call amovs (Mems[input+lb], Mems[zs], count)
		call amovs (Mems[input], Mems[zs+count], lb)
	    }
	    # Now zoom it
	    if (zm == 1)
		call amovs (Mems[zs], Mems[input], IIS_XDIM)
	    else
		call ids_blockit (Mems[zs+IIS_XCEN-IIS_XCEN/zm], Mems[input],
		              IIS_XDIM, real(zm))
	}

	if (i == GRCHNUM)
	    return

	# With the aligned data, perform the lookup.  Note that left is
	# 0 based, right is (0-based) first excluded value.

	do j = sn_start, sn_end {
	    if (left[j, yi, i] == -1)
		next
	    itemp = left[j,yi,i]
	    x1 = max (itemp, xstart)
	    itemp = right[j,yi,i]
	    x2 = min (itemp - 1, xend)
	    call aluts (Mems[input+x1], Mems[zs], x2-x1+1, Mems[lutp[j,i]])
	    call aadds (Mems[zs], Mems[result[j]+x1], Mems[result[j]+x1],
	   		x2-x1+1)
	}
end

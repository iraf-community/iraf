#  IG_STEP -- Draw a stepped curve, i.e.,  A histogram without the
#  vertical lines 

#  Plot all points even if they fall outside viewport, allowing
#  clipping to remove them.  Previously, the points were removed before
#  plotting, resulting in gaps in the curve.  3/15/91  ZGL

#  Add an argument to STEP to allow the user to choose whether to plot
#  out of bounds poins.  3/15/91  ZGL

#  8/20/91 Removed ^Ls. ZGL
#  4/7/93  Modified to work with FLIPped axes.

include <gset.h>
include "igi.h"

procedure ig_step (igs)

pointer	igs

int	token
int	clip		# Clip rather than ignore out of bounds points?

int	gettok()

begin
	call lcmdcat (igs, YES)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token))
	    # No argument;  allow viewport to clip off-scale points
	    clip = YES
	else
	    # Argument;  remove off-scale points
	    clip = NO

	if (MG_YDATAP(PLOT_PARMS(igs)) == NULL) {
	    call error (0,"No Y data ")
	    return
	}

	call ii_step (igs, clip)

	call cmdcat (igs, YES)
end


#  II_STEP -- Draw a stepped curve, i.e.,  A histogram without the
#  vertical lines 

procedure ii_step (igs, clip)

pointer	igs
int	clip		# Clip rather than ignore out of bounds points?

pointer	igps
int	npts

begin
	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL)
	    return

	call setltype (igs, MG_LTYPEN(igps))
	call gseti (GIO_GP(igs), G_CLIP, YES)

	if (MG_XDATAP(igps) == NULL)
	    # No X data;  use pixel numbers
	    call stp_vline (GIO_GP(igs), 
		Memr[MG_YDATAP(igps)], MG_YNPTS(igps), clip)
	else {
	    # X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps)) 
	    call stp_line (GIO_GP(igs), Memr[MG_XDATAP(igps)], 
		Memr[MG_YDATAP(igps)], npts, clip)
	}

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end


#  STP_LINE -- Draw a pseudo-histogram line (stepped curve) through the
#  points. 

procedure stp_line (gp, xdata, ydata, npts, clip)

pointer	gp		# Graphics descriptor
real	xdata[ARB]	# X coordinates of the line endpoints
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints
int	clip		# Clip rather than ignore out of bounds points?

real	x, y
real	left, right, bottom, top
real	xmin, xmax, ymin, ymax
real	dx, xp
int	pixel
bool	lgood
bool	bclip

begin
	# Find the current data window
	call ggwind (gp, left, right, bottom, top)

	xmin = min (left, right)
	xmax = max (left, right)
	ymin = min (bottom, top)
	ymax = max (bottom, top)

	bclip = clip == YES

	# Do the first point
	x = xdata[1]
	y = ydata[1]

	if (!IS_INDEF(x) && !IS_INDEF(y) && 
	    x >= xmin && x <= xmax && 
	    y >= ymin && y <= ymax) {

	    # Data point is valid and inside plot limits;  
	    # make the first point centered on the first horizontal segment

	    dx = (xdata[2] - x) / 2.0
	    xp = x - dx
	    xp = max (xp, xmin)
	    call gamove (gp, xp, y)
	    xp = x + dx
	    call gadraw (gp, xp, y)
	    lgood = true

	} else
	    # Data point is invalid
	    lgood = false

	do pixel = 2, npts - 1 {
	    # For each point in the current curve
	    x = xdata[pixel]
	    y = ydata[pixel]

	    if (!bclip && (y < ymin || y > ymax)) {
		lgood = false
		next
	    }

	    if (!IS_INDEF(x) && !IS_INDEF(y) && 
		x >= xmin && x <= xmax) {

		# Data point is valid
		dx = (x - xdata[pixel-1]) / 2.0
		xp = x - dx

		if (lgood) 
		    # The last point was valid;
		    # draw the vertical connection
		    call gadraw (gp, xp, y)
		else {
		    # The last point was not valid;
		    # start a new segment
		    xp = max (xp, xmin)
		    call gamove (gp, xp, y)
		}
		# Draw the horizontal segment
		if (IS_INDEF(ydata[pixel+1])) 
		    # The next pixel is not valid;  
		    # use the offset to the last pixel
		    xp = x + dx
		else
		    # The next pixel is valid;  
		    # use the offset to the next pixel
		    xp = (x + xdata[pixel+1]) / 2.0

		xp = min (xp, xmax)
		call gadraw (gp, xp, y)

		lgood = true
	    } else
		# Data point is indefinite (not valid)
		lgood = false
	}

	# Do the last point
	x = xdata[npts]
	y = ydata[npts]

	if (!IS_INDEF(x) && !IS_INDEF(y) && 
	    x >= xmin && x <= xmax && 
	    y >= ymin && y <= ymax) {

	    # Data point is valid;  
	    # make the first point centered on the first horizontal segment

	    dx = (x - xdata[npts-1]) / 2.0
	    xp = x - dx

	    if (lgood) 
		# The last point was valid;  
		# draw the vertical connection
		call gadraw (gp, xp, y)
	    else
		# The last point was masked;  start a new segment
		call gamove (gp, xp, y)

	    # Draw the last horizontal segment
	    xp = x + dx
	    xp = min (xp, xmax)
	    call gadraw (gp, xp, y)
	}
end


#  STP_VLINE -- Draw a pseudo-histogram line (stepped curve) through the
#  points. 

procedure stp_vline (gp, ydata, npts, clip)

pointer	gp		# Graphics descriptor
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints
int	clip		# Clip rather than ignore out of bounds points?

real	x, y
real	left, right, bottom, top
real	xmin, xmax, ymin, ymax
real	dx, xp
int	pixel
bool	lgood
bool	bclip

begin
	# Find the current data window
	call ggwind (gp, left, right, bottom, top)

	xmin = min (left, right)
	xmax = max (left, right)
	ymin = min (bottom, top)
	ymax = max (bottom, top)

	bclip = clip == YES

	# Do the first point
	x = 1.0
	y = ydata[1]

	if (!IS_INDEF(y) && 
	    x >= xmin && x <= xmax && 
	    y >= ymin && y <= ymax) {

	    # Data point is valid and inside plot limits;  
	    # make the first point centered on the first horizontal segment

	    dx = 1.0 - x / 2.0
	    xp = x - dx
	    xp = max (xp, xmin)
	    call gamove (gp, xp, y)
	    xp = x + dx
	    call gadraw (gp, xp, y)
	    lgood = true

	} else
	    # Data point is invalid
	    lgood = false

	do pixel = 2, npts - 1 {
	    # For each point in the current curve
	    x = real (pixel)
	    y = ydata[pixel]

	    if (!bclip && (y < ymin || y > ymax)) {
		lgood = false
		next
	    }

	    if (!IS_INDEF(y) && 
		x >= xmin   && x <= xmax) {

		# Data point is valid
		dx = (x - real (pixel-1)) / 2.0
		xp = x - dx

		if (lgood) 
		    # The last point was valid;
		    # draw the vertical connection
		    call gadraw (gp, xp, y)
		else {
		    # The last point was not valid;
		    # start a new segment
		    xp = max (xp, xmin)
		    call gamove (gp, xp, y)
		}
		# Draw the horizontal segment
		if (IS_INDEF(ydata[pixel+1])) 
		    # The next pixel is not valid;  
		    # use the offset to the last pixel
		    xp = x + dx
		else
		    # The next pixel is valid;  
		    # use the offset to the next pixel
		    xp = (x + real (pixel+1)) / 2.0

		xp = min (xp, xmax)
		call gadraw (gp, xp, y)

		lgood = true
	    } else
		# Data point is indefinite (not valid)
		lgood = false
	}

	# Do the last point
	x = real (npts)
	y = ydata[npts]

	if (!IS_INDEF(y) && 
	    x >= xmin && x <= xmax && 
	    y >= ymin && y <= ymax) {

	    # Data point is valid;  
	    # make the first point centered on the first horizontal segment

	    dx = (x - real (npts-1)) / 2.0
	    xp = x - dx

	    if (lgood) 
		# The last point was valid;  
		# draw the vertical connection
		call gadraw (gp, xp, y)
	    else
		# The last point was masked;  start a new segment
		call gamove (gp, xp, y)

	    # Draw the last horizontal segment
	    xp = x + dx
	    xp = min (xp, xmax)
	    call gadraw (gp, xp, y)
	}
end

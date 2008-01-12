#  HGLINE -- Draw a pseudo-histogram line (stepped curve) through the
#  points. 

procedure hgline (gp, xdata, ydata, npts)

pointer	gp		# Graphics descriptor
real	xdata[ARB]	# X coordinates of the line endpoints
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints

real	x, y
real	left, right, bottom, top
real	dx, xp
int	pixel
bool	lgood

begin
	# Find the current data window
	call ggwind (gp, left, right, bottom, top)

	# Do the first point
	x = xdata[1]
	y = ydata[1]

	if (!IS_INDEF(x) && !IS_INDEF(y) && 
	    x >= left   && x <= right && 
	    y >= bottom && y <= top) {

	    # Data point is valid and inside plot limits;  
	    # make the first point centered on the first horizontal segment

	    dx = (xdata[2] - x) / 2.0
	    xp = x - dx
	    xp = max (xp, left)
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

	    if (!IS_INDEF(x) && !IS_INDEF(y) && 
		x >= left   && x <= right && 
		y >= bottom && y <= top) {

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
		    xp = max (xp, left)
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

		xp = min (xp, right)
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
	    x >= left   && x <= right && 
	    y >= bottom && y <= top) {

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
	    xp = min (xp, right)
	    call gadraw (gp, xp, y)
	}
end

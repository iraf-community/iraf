include <mach.h>
include	<gset.h>
include	<gio.h>

define	MSIZE 2.0

# GT_DELPT -- Mark a point as deleted

procedure gt_delpt (gd, wx, wy, x, y, npix, deleted, undelete)

pointer	gd				# Graphics descriptor
real	wx				# Cursor position
real	wy				#   ""
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete			# Undelete flag

int	row, i
real	r2min, r2, x0, y0

begin
	# Search for the nearest point which has not been deleted

	row = 0
	r2min = MAX_REAL

	# Transform  world cursor coordintes to NDC
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    call gctran (gd, x[i], y[i], x0, y0, 1, 0)
	    if (x[i] < INDEFR && y[i] < INDEFR)
	        r2 = (wx - x0) ** 2 + (wy - y0) ** 2
	    else
		r2 = MAX_REAL

	    if (r2 < r2min) {
		r2min = r2
		row = i
	    }
	}

	if (row != 0) {
	    # Mark row as being deleted
	    if (undelete == NO) {
	        # Plot X over point
	        call gscur (gd, x[row], y[row])
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[row], y[row], GM_CROSS, MSIZE, MSIZE)
	        deleted[row] = YES
	    } else {
		deleted[row] = NO
	        call gscur (gd, x[row], y[row])
		call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[row], y[row], GM_CROSS, MSIZE, MSIZE)
	    }

	}

end

# GT_DYGT -- Delete all point > input Y

procedure gt_dygt (gd, wy, x, y, npix, deleted, undelete)

pointer	gd				# Graphics descriptor
real	wy				# Cursor position
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete

int	i

begin
	# Search for points with Y values > than the critical value

	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (y[i] > wy) {
		if (undelete == NO) {
	            # Plot X over point
		    call gseti (gd, G_PMLTYPE, GL_SOLID)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    deleted[i] = YES
		} else {
		    deleted[i] = NO
		    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		}
	    }
	}
end

# GT_DYLT -- Delete all point < input Y

procedure gt_dylt (gd, wy, x, y, npix, deleted, undelete)

pointer	gd				# Graphics descriptor
real	wy				# Cursor position
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete

int	i

begin
	# Search for points with Y values > than the critical value

	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (y[i] < wy) {
		if (undelete == NO) {
		    deleted[i] = YES
	            # Plot X over point
		    call gseti (gd, G_PMLTYPE, GL_SOLID)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		} else {
		    deleted[i] = NO
		    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		}
	    }
	}
end

# GT_DXGT -- Delete all point > input X

procedure gt_dxgt (gd, wx, x, y, npix, deleted, undelete)

pointer	gd				# Graphics descriptor
real	wx				# Cursor position
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete

int	i

begin
	# Search for points with X values > than the critical value

	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (x[i] > wx) {
		if (undelete == NO) {
		    deleted[i] = YES
	            # Plot X over point
		    call gseti (gd, G_PMLTYPE, GL_SOLID)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		} else {
		    deleted[i] = NO
		    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		}
	    }
	}
end

# GT_DXLT -- Delete all point > input Y

procedure gt_dxlt (gd, wx, x, y, npix, deleted, undelete)

pointer	gd				# Graphics descriptor
real	wx				# Cursor position
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete

int	i

begin
    # Search for points with X values < than the critical value

	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (x[i] < wx) {
		if (undelete == NO) {
		    deleted[i] = YES
	            # Plot X over point
		    call gseti (gd, G_PMLTYPE, GL_SOLID)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		} else {
		    deleted[i] = NO
		    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	            call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		}
	    }
	}
end

# GT_DBOX -- Delete all point in a box

procedure gt_dbox (gd, npix, deleted, undelete, x, y, x1, y1, x2, y2)

pointer	gd				# Graphics descriptor
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
real	x1, y1, x2, y2			# Corners of the box

int	i
real	temp

begin
        # Make sure the points are in  the correct order
        if (y2 < y1) {
	    temp = y1
	    y1 = y2
	    y2 = temp
	}
        if (x2 < x1) {
	    temp = x1
	    x1 = x2
	    x2 = temp
	}
        # Search for points within the box
	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (x[i] <= x2 && x[i] >= x1) {
		if (y[i] <= y2 && y[i] >= y1) {
		    if (undelete == NO) {
		        deleted[i] = YES
	                # Plot X over point
			call gseti (gd, G_PMLTYPE, GL_SOLID)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    } else {
			deleted[i] = NO
		        call gseti (gd, G_PMLTYPE, GL_CLEAR)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    }
		}
	    }
	}
end

# GT_DSEG -- Delete all point on one side of a line segment

procedure gt_dseg (gd, npix, deleted, undelete, x, y, x1, y1, x2, y2, x0, y0)

pointer	gd				# Graphics descriptor
int	npix				# # of pixels
int	deleted[ARB]			# Array of delete indicators
int	undelete
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
real	x1, y1, x2, y2			# Corners of the box
real	x0, y0, slope, inter, temp

int	i, lessthan

begin
        # Make sure the points are in  the correct order
        if (y2 < y1) {
	    temp = y1
	    y1 = y2
	    y2 = temp
	}
        if (x2 < x1) {
	    temp = x1
	    x1 = x2
	    x2 = temp
	}

	# Calculate slope and intercept
	slope = (y2 - y1) / (x2 - x1)
	inter = (x2 * y1 - x1 * y2) / (x2 - x1)

	# Which side should we delete the lines from?
	temp = x0 * slope + inter
	if (temp <= y0)
	    lessthan = NO
	else
	    lessthan = YES

        # Search for points with X values between x1 and x2
	do i = 1 , npix {
	    if ((deleted[i] == YES && undelete == NO) ||
		(deleted[i] == NO && undelete == YES))
		next
	    
	    if (x[i] <= x2 && x[i] >= x1) {
		# Now which side of the line does this point fall on
		temp = x[i] * slope + inter
		if (y[i] < temp && lessthan == YES) {
		    if (undelete == NO) {
		        deleted[i] = YES
	                # Plot X over point
			call gseti (gd, G_PMLTYPE, GL_SOLID)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    } else {
			deleted[i] = NO
		        call gseti (gd, G_PMLTYPE, GL_CLEAR)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    }
		} else if (y[i] > temp && lessthan == NO) {
		    if (undelete == NO) {
		        deleted[i] = YES
	                # Plot X over point
			call gseti (gd, G_PMLTYPE, GL_SOLID)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    } else {
			deleted[i] = NO
		        call gseti (gd, G_PMLTYPE, GL_CLEAR)
	                call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
		    }
		}
	    }
	}
end

# GT_PDEL -- Overplot crosses on those points which have been marked for zap.

procedure gt_pdel (gd, x, y, deleted, npix)

pointer	gd
real	x[ARB]
real	y[ARB]
int	deleted[ARB]
int	npix

int 	i

begin
	do i = 1, npix {
	    if (deleted[i] == YES) {
	        # Plot X over point
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end

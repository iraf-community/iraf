include	<gset.h>
include <mach.h>
include "pexamine.h"

define	MSIZE 2.0
 
# PT_DELPT -- Mark/unmark the point  nearest the cursor.

procedure pt_delpt (gd, wx, wy, xpos, ypos, x, y, deleted, npix, undelete,
	matchrad)

pointer	gd			# pointer to the graphics stream
real	wx			# X cursor position
real	wy			# Y cursor position
real	xpos[ARB]		# X coordinate array
real	ypos[ARB]		# Y coordinate array
real	x[ARB]			# X array of plotted data
real	y[ARB]			# Y array of plotted data
int	deleted[ARB]		# array of delete indicators
int	npix			# number of pixels
int	undelete		# undelete flag
real	matchrad		# matching radius

int	i, row
real	r2min, r2, mr2, wx0, wy0, xpos0, ypos0

begin
	# Initialize.
	row = 0
	r2min = MAX_REAL

	# Set matching radius for the image display or graph.
	if (IS_INDEFR(matchrad)) {
	    mr2 = MAX_REAL
	    if (gd != NULL)
	        call gctran (gd, wx, wy, wx0, wy0, 1, 0)
	    else {
	        wx0 = wx
	        wy0 = wy
	    }
	} else {
	    mr2 = matchrad ** 2
	    wx0 = wx
	    wy0 = wy
	}

	# Search for the point nearest the cursor.
	do i = 1 , npix {

	    if (deleted[i] == PX_DELETE)
		next

	    if ((deleted[i] == PX_MARK && undelete == NO) ||
		(deleted[i] == PX_GOOD && undelete == YES))
		next

	    if (! IS_INDEFR(xpos[i]) && ! IS_INDEFR(ypos[i])) {
	        if (IS_INDEFR(matchrad)) {
		    if (gd != NULL)
	                call gctran (gd, xpos[i], ypos[i], xpos0, ypos0, 1, 0)
		    else {
	                xpos0 = xpos[i]
	                ypos0 = ypos[i]
		    }
	        } else {
	            xpos0 = xpos[i]
	            ypos0 = ypos[i]
	        }
	        r2 = (wx0 - xpos0) ** 2 + (wy0 - ypos0) ** 2
	    } else
		r2 = MAX_REAL
	    if (r2 >= r2min)
		next

	    r2min = r2
	    row = i
	}

	# Return if point not found.
	if ((row == 0) || (r2min > mr2))
	    return

	# Delete the point.
	if (undelete == NO) {
	    deleted[row] = PX_MARK
	    if (gd == NULL)
		return
	    call gseti (gd, G_PMLTYPE, GL_SOLID)
	    call gmark (gd, x[row], y[row], GM_CROSS, MSIZE, MSIZE)
	} else {
	    deleted[row] = PX_GOOD
	    if (gd == NULL)
		return
	    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	    call gmark (gd, x[row], y[row], GM_CROSS, MSIZE, MSIZE)
	}
end
 

# PT_DYGTG -- Delete all points with Y >  Y (cursor).

procedure pt_dygtg (gd, wy, ypos, x, y, deleted, npix, undelete)

pointer	gd			# pointer to graphics stream
real	wy			# Y cursor position
real	ypos[ARB]		# Y array of coordinate data
real	x[ARB]			# X array of plotted data
real	y[ARB]			# Y array of plotted data
int	deleted[ARB]		# array of delete indicators
int	npix			# number of pixels
int	undelete		# the delete or undelete flag

int	i

begin
	do i = 1 , npix {
	    if (ypos[i] <= wy)
		next
	    if ((deleted[i] == PX_GOOD) && (undelete == NO)) {
		deleted[i] = PX_MARK
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    } else if ((deleted[i] == PX_MARK) && (undelete == YES)) {
		deleted[i] = PX_GOOD
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end


# PT_DYLTG -- Delete all points with Y < Y (cursor).

procedure pt_dyltg (gd, wy, ypos, x, y, deleted, npix, undelete)

pointer	gd			# pointer to graphics stream
real	wy			# Y cursor position
real	ypos[ARB]		# Y array of coordinate data
real	x[ARB]			# X array of plotted data
real	y[ARB]			# Y array of plotted data
int	deleted[ARB]		# array of delete indicators
int	npix			# number of pixels
int	undelete		# the delete or undelete flag

int	i

begin
	do i = 1 , npix {
	    if (ypos[i] >= wy)
		next
	    if ((deleted[i] == PX_GOOD) && (undelete == NO)) {
		deleted[i] = PX_MARK
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    } else if ((deleted[i] == PX_MARK) && (undelete == YES)) {
		deleted[i] = PX_GOOD
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end


# PT_DXGTG -- Mark delete for all points with X > X (cursor)

procedure pt_dxgtg (gd, wx, xpos, x, y, deleted, npix, undelete)

pointer	gd			# pointer to graphics stream
real	wx			# X cursor position
real	xpos[ARB]		# X coordinate array
real	x[ARB]			# X array of plotted data
real	y[ARB]			# Y array of plotted data
int	deleted[ARB]		# array of delete indicators
int	npix			# number of pixels
int	undelete		# the delete or undelete flag

int	i

begin
	do i = 1 , npix {
	    if (xpos[i] <= wx)
		next
	    if ((deleted[i] == PX_GOOD) && (undelete == NO)) {
		deleted[i] = PX_MARK
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    } else if ((deleted[i] == PX_MARK) && (undelete == YES)) {
		deleted[i] = PX_GOOD
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end


# PT_DXLTG -- Mark delete for all points with X < X (cursor).

procedure pt_dxltg (gd, wx, xpos, x, y, deleted, npix, undelete)

pointer	gd			# pointer to graphics stream
real	wx			# X cursor position
real	xpos[ARB]		# X coordinate array
real	x[ARB]			# X array of plotted data
real	y[ARB]			# Y array of plotted data
int	deleted[ARB]		# array of delete indicators
int	npix			# number of pixels
int	undelete		# the delete or undelete flag

int	i

begin
	do i = 1 , npix {
	    if (xpos[i] >= wx)
		next
	    if ((deleted[i] == PX_GOOD) && (undelete == NO)) {
		deleted[i] = PX_MARK
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    } else if ((deleted[i] == PX_MARK) && (undelete == YES)) {
		deleted[i] = PX_GOOD
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end


# PT_DBOXG -- Delete all points inside a box

procedure pt_dboxg (gd, xpos, ypos, x, y, deleted, npix, x1, y1, x2, y2,
	undelete)

pointer	gd			# pointer to the graphics stream
real	xpos[ARB]		# x coordinate array
real	ypos[ARB]		# y coordinate array
real	x[ARB]			# x array of plotted data
real	y[ARB]			# y array of plotted data
int	deleted[ARB]		# array of deletion indicators
int	npix			# number of pixels
real	x1, y1, x2, y2		# corners of the box
int	undelete		# delete or undelete points

int	i
real	temp

begin
        # Make sure the points are in the correct order.
        if (x2 < x1) {
	    temp = x1
	    x1 = x2
	    x2 = temp
	}
        if (y2 < y1) {
	    temp = y1
	    y1 = y2
	    y2 = temp
	}

        # Search for points within the box and delete.
	do i = 1 , npix {
	    if (xpos[i] < x1 || xpos[i] > x2 || ypos[i] < y1 || ypos[i] > y2)
		next
	    if ((deleted[i] == PX_GOOD) && (undelete == NO)) {
	        deleted[i] = PX_MARK
		if (gd == NULL)
		    next
		call gseti (gd, G_PMLTYPE, GL_SOLID)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    } else if ((deleted[i] == PX_MARK) && (undelete == YES)) {
		deleted[i] = PX_GOOD
		if (gd == NULL)
		    next
	        call gseti (gd, G_PMLTYPE, GL_CLEAR)
	        call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	    }
	}
end


# PT_MDELETE -- Overplot crosses on those points which have been marked for
# deletion.

procedure pt_mdelete (gd, x, y, deleted, npix)

pointer	gd			# pointer to the graphics stream
real	x[ARB]			# the array plotted along the x axis
real	y[ARB]			# the array plotted along the y axis
int	deleted[ARB]		# the array of deletion indices
int	npix			# the number of pixels

int 	i

begin
	do i = 1, npix {
	    if (deleted[i] != PX_MARK)
		next
	    call gseti (gd, G_PMLTYPE, GL_SOLID)
	    call gmark (gd, x[i], y[i], GM_CROSS, MSIZE, MSIZE)
	}
end


# PT_UPDATE -- Actually delete points currently marked for deletion.

procedure pt_update (deleted, npix)

int	deleted[ARB]		# array of deletions indices
int	npix			# the number of pixels

int	i

begin
	# Add the marked points to the deletions array.
	do i = 1, npix {
	    if (deleted[i] != PX_MARK)
		next
	    deleted[i] = PX_DELETE
	}
end

include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>

# AUTO_EXP -- Auto expand around the marked region

procedure auto_exp (gp, gt, key, wx1, x, y, n)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
int	key		# Key
real	wx1		# Cursor position
real	x[n]		# Pixel coordinates
real	y[n]		# Pixel data for Y scaling
int	n		# Number of pixels

char	cmd[1]
int	i, wcs
real	x1, x2, y1, y2, wx2, wy, dx, xmin, xmax, ymin, ymax

int	clgcur()

begin
	# Get the current window.
	call ggwind (gp, x1, x2, y1, y2)

	# Compute the new window in x.
	dx = x2 - x1
	switch (key) {
	case 'a':	# Expand
	    call printf ("again:\n")
	    i = clgcur ("cursor", wx2, wy, wcs, key, cmd, SZ_LINE)
	    x1 = wx1
	    x2 = wx2
	case ',':	# Shift left
	    x1 = x1 - 0.85 * dx
	    x2 = x2 - 0.85 * dx
	case '.':	# Shift right
	    x1 = x1 + 0.85 * dx
	    x2 = x2 + 0.85 * dx
	case 'z':	# Zoom x axis
	    x1 = x1 + 0.25 * dx
	    x2 = x2 - 0.25 * dx
	}

	if (x1 == x2) {
	    # Autoscale.
	    x1 = INDEF
	    x2 = INDEF
	    ymin = INDEF
	    ymax = INDEF
	} else {
	    # Determine the y limits for pixels between x1 and x2.
	    xmin = min (x1, x2)
	    xmax = max (x1, x2)
	    ymin = MAX_REAL
	    ymax = -MAX_REAL
	    do i = 1, n {
		if (x[i] < xmin || x[i] > xmax)
		    next
		ymin = min (y[i], ymin)
		ymax = max (y[i], ymax)
	    }
	    if (ymin > ymax) {
		ymin = y1
		ymax = y2
	    } else if (y1 > y2) {
		y1 = ymin
		ymin = ymax
		ymax = y1
	    }
	}

	call gt_setr (gt, GTXMIN, x1)
	call gt_setr (gt, GTXMAX, x2)
	call gt_setr (gt, GTYMIN, ymin)
	call gt_setr (gt, GTYMAX, ymax)
	call replot (gp, gt, x, y, n, YES)
end

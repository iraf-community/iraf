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
int	i1, i2, npts, wcs
real	wx2, wy, x1, x2, y1, y2, dx

int	clgcur()

begin
	# Get the current window.
	call ggwind (gp, x1, x2, y1, y2)
	dx = x2 - x1

	# Compute the new window in x.
	switch (key) {
	case 'a':	# Expand
	    call printf ("again:\n")
	    i1 = clgcur ("cursor", wx2, wy, wcs, key, cmd, SZ_LINE)
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

	# Determine the y limits for pixels between x1 and x2.
	dx = (x[n] - x[1]) / (n - 1)
	i1 = (x1 - x[1]) / dx + 1.5
	i2 = (x2 - x[1]) / dx + 1.5
	i1 = max (1, min (n, i1))
	i2 = max (1, min (n, i2))
	if (i1 < i2) {
	    npts = i2 - i1 + 1
	    call alimr (y[i1], npts, y1, y2)
	} else {
	    npts = i1 - i2 + 1
	    call alimr (y[i2], npts, y1, y2)
	}

	call gt_setr (gt, GTXMIN, x1)
	call gt_setr (gt, GTXMAX, x2)
	call gt_setr (gt, GTYMIN, y1)
	call gt_setr (gt, GTYMAX, y2)
	call replot (gp, gt, x, y, n, YES)
end

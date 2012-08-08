# DP_GLIM -- Get the lower and upper limits of a section around a specified
# center.

procedure dp_glim (xc, yc, radius, ixmin, ixmax, iymin, iymax, lx, mx, ly, my)

real	xc, yc			# the x and y center points
real	radius			# the radial distance
int	ixmin, ixmax		# absolute x boundaries
int	iymin, iymax		# absolute y boundaries
int	lx, mx, ly, my		# the returned limits

begin
        lx = max (ixmin - 1, min (ixmax, int (xc - radius))) + 1
        mx = max (ixmin, min (ixmax, int (xc + radius)))
        ly = max (iymin - 1, min (iymax, int (yc - radius))) + 1
        my = max (iymin, min (iymax, int (yc + radius)))
end

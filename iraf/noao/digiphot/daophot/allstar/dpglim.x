# DP_GLIM -- Get the lower and upper limits of a section around a specified
# center.

procedure dp_glim (xc, yc, radius, ixmin, ixmax, iymin, iymax, lx, mx, ly, my)

real	xc, yc			# the x and y center points
real	radius			# the radial distance
long	ixmin, ixmax		# absolute x boundaries
long	iymin, iymax		# absolute y boundaries
long	lx, mx, ly, my		# the returned limits

long	lint()

begin
        lx = max (ixmin - 1, min (ixmax, lint (xc - radius))) + 1
        mx = max (ixmin, min (ixmax, lint (xc + radius)))
        ly = max (iymin - 1, min (iymax, lint (yc - radius))) + 1
        my = max (iymin, min (iymax, lint (yc + radius)))
end

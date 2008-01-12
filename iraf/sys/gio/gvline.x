# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GVLINE -- Vector polyline.  Draw a line connecting the points (X[i],V[i]),
# where the X[i] are evenly distributed from X1 to X2.

procedure gvline (gp, v, npts, x1, x2)

pointer	gp			# graphics descriptor
real	v[ARB]			# Y coordinates of the polyline
int	npts			# number of polyline points
real	x1, x2			# range of X coordinates of the polyline

int	i
real	dx

begin
	if (npts > 1) {
	    dx = (x2 - x1) / (npts - 1)
	    call gamove (gp, x1, v[1])
	    do i = 2, npts
		call gadraw (gp, (i-1) * dx + x1, v[i])
	}
end

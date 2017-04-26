# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GPLINE -- Polyline.  Draw a line connecting the points (X[i],Y[i]), i.e.,
# move to the first point and draw a straight line from there to the second
# point, from the second to the third, and so on.

procedure gpline (gp, x, y, npts)

pointer	gp			# graphics descriptor
real	x[ARB], y[ARB]		# points defining the polyline
int	npts
int	i

begin
	call gamove (gp, x[1], y[1])
	do i = 2, npts
	    call gadraw (gp, x[i], y[i])
end

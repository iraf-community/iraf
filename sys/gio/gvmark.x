# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>

# GVMARK -- Vector polymarker.  Output at sequence of markers at the vertices
# of a polygon, all markers the same type and size.  The polygon is given by
# the set of points (X[i],V[i]), where the X[i] are evenly distributed from X1
# to X2.  The marker type GM_POINT is a special case.

procedure gvmark (gp, v, npts, x1, x2, marktype, xsize, ysize)

pointer	gp			# graphics descriptor
real	v[ARB]			# Y[i] polygon
int	npts			# number of points
real	x1, x2			# range of X[i]
int	marktype		# marker type
real	xsize, ysize		# marker size

int	i
real	dx

begin
	if (npts > 1)
	    if (marktype == GM_POINT) {
		call gpl_settype (gp, POLYMARKER)
		call gvline (gp, v, npts, x1, x2)
		call gpl_settype (gp, POLYLINE)
	    } else {
		dx = (x2 - x1) / (npts - 1)
		do i = 1, npts
		    call gmark (gp, (i-1) * dx + x1, v[i], marktype,
			xsize, ysize)
	    }
end

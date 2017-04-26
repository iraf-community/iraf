# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>

# GPMARK -- Polymarker.  Output at sequence of markers at the vertices of a
# polygon, all markers the same type and size.  The marker type GM_POINT is
# a special case.

procedure gpmark (gp, x, y, npts, marktype, xsize, ysize)

pointer	gp			# graphics descriptor
real	x[ARB], y[ARB]		# vertices of polygon
int	npts			# number of points
int	marktype		# marker type
real	xsize, ysize		# marker size
int	i

begin
	if (marktype == GM_POINT) {
	    call gpl_settype (gp, POINTMODE)
	    call gpline (gp, x, y, npts)
	    call gpl_settype (gp, POLYLINE)
	} else {
	    do i = 1, npts
		call gmark (gp, x[i], y[i], marktype, xsize, ysize)
	}
end

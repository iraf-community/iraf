# GPL.COM -- Polyline generator common.

bool	last_point_inbounds	# last point was inbounds
int	xtran, ytran		# scaling function for X, Y axes (linear,log,,)
int	op			# index of next cell in polyline array
int	pl_type			# type of instruction (polyline, polymarker,...)
int	pl_pointmode		# plotting points (polymarker), not vectors
int	wcs			# WCS for which cache is valid
long	mxorigin, myorigin	# origin in world coordinates for transform
real	wxorigin, wyorigin	# origin in world coordinates for transform
real	xscale, yscale		# scale factor, world to GKI, for transform
real	cx, cy			# current pen position, world coords
long	mx1, mx2, my1, my2	# clipping viewport, GKI coords
long	xs[4], ys[4]		# last point plotted (for clipping code)
pointer	gp_out			# device which owns current polyline
short	pl[LEN_PLBUF]		# output polyline buffer

common	/gplcom/ last_point_inbounds, xtran, ytran, op, pl_type, pl_pointmode,
	mxorigin, myorigin, wxorigin, wyorigin, xscale, yscale, cx, cy,
	mx1, mx2, my1, my2, xs, ys, gp_out, wcs, pl

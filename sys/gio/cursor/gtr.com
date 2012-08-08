# GTR.COM -- Polyline clipping common for the workstation transformation.
# The length of this common in integer units from startcom to endcom inclusive
# is a defined parameter in giotr.h.  Values within the save area are saved
# in the TR descriptor for a device and loaded into the common (which serves
# as a cache) when GIOTR or RCURSOR is called for a device.  LENGTH=28

pointer	trdes[MAX_PSEUDOFILES]	# pointers to giotr descriptors
int	tr_stream		# graphics stream currently in the cache
int	startcom		# dummy entry marking start of common
int	pl_op			# index of next cell in polyline array
bool	last_point_inbounds	# last point was inbounds
int	pl_type			# type of instruction (polyline, polymarker,...)
int	wstranset		# workstation transformation has been set
real	xscale, yscale		# scale factor, world to GKI, for transform
real	xorigin, yorigin	# origins in GKI coords, for transform
long	cx, cy			# current pen position, GKI coords
long	mx1, mx2, my1, my2	# clipping viewport, GKI coords
real	vx1, vx2, vy1, vy2	# NDC viewport, may extend beyond boundary
long	xs[4], ys[4]		# last point plotted (for clipping code)
int	endcom			# dummy entry marking end of saved area
short	pl[LEN_PLBUF+5]		# output polyline buffer (plus GKI header)

common	/gtrcom/ trdes, tr_stream, startcom, pl_op, last_point_inbounds,
	pl_type, wstranset, xscale, yscale, xorigin, yorigin, cx, cy,
	mx1, mx2, my1, my2, vx1, vx2, vy1, vy2, xs, ys, endcom, pl

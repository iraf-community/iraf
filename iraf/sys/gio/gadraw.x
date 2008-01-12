# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

define	MOVE		0
define	DRAW		1

# GADRAW -- Draw absolute.  This is the primary line drawing primitive, used
# to transform and clip polylines, polymarkers, and polygons (fill area).
# Our function is to handle INDEFS, the normalization transformation, and
# clipping, building up a polyline in GKI coordinates.  Each call processes
# a point of the input polyline, adding zero, one, or two points to the output
# clipped polyline, which is buffered internally in the static polyline buffer
# PL.  Plotting an INDEF terminates the polyline and starts a new one, causing
# a gap to appear in the plotted polyline.  Long polylines are broken up into
# shorter polylines to simplify buffering.  The transformation parameters are
# computed and cached in the GPL common for maximum efficiency.

procedure gadraw (gp, wx, wy)

pointer	gp			# graphics descriptor
real	wx, wy			# absolute world coordinates of next point

int	i
real	x, y
long	mx, my
bool	inbounds
include	"gpl.com"

begin
	# Update cached transformation parameters if device changes or cache
	# is invalidated by setting gp_out to null.  If the WCS changes it
	# is not necessary to flush the polyline but we must update the
	# cached transformation parameters.

	if (gp != gp_out) {
	    call gpl_flush()
	    call gpl_cache (gp)
	} else if (GP_WCS(gp) != wcs)
	    call gpl_cache (gp)

	# Break polyline (visible break in the plotted line) if point is
	# indefinite.

	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
	    call gamove (gp, wx, wy)
	    return
	}

	# Transform point (wx,wy) to long integer NDC coordinates in the range
	# 0 to GKI_MAXNDC.  This combines the WCS->NDC->GKI transformations into
	# a single transformation and permits use of integer arithmetic for
	# clipping.  Long integer arithmetic is necessary to provide sufficient
	# precision to represent GKI_MAXNDC**2, the largest possible integer
	# value in an expression.

	if (xtran == LINEAR && ytran == LINEAR) {
	    # Optimize the case linear.
            x = max (0.0, min (real(GKI_MAXNDC),
                ((wx - wxorigin) * xscale) + mxorigin))
            y = max (0.0, min (real(GKI_MAXNDC),
                ((wy - wyorigin) * yscale) + myorigin))
	} else {
	    # General case.
	    call gpl_wcstogki (gp, wx, wy, x, y)
	}

	# Check to see if this is the first point of a new polyline.  If so we
	# must set the first physical point in the output polyline to the
	# current position, making the current point the second physical point
	# of the output polyline.  If the current position is indefinite
	# then we take the current point to define the current position and
	# it is put into the polyline on the next call.

	if (op == 1) {
	    if (IS_INDEF(cx) || IS_INDEF(cy)) {
		cx = x
		cy = y
		return

	    } else {
		# Place the current pen position in the polyline as the
		# first point if it is inbounds.

		mx = cx
		my = cy
		if (my <= my2 && my >= my1 && mx <= mx2 && mx >= mx1) {
		    last_point_inbounds = true
		    pl[op] = mx
		    op = op + 1
		    pl[op] = my
		    op = op + 1
		} else {
		    last_point_inbounds = false
		    do i = 1, 4 {
			xs[i] = cx
			ys[i] = cy
		    }
		}
	    }
	}

	# Update the current position, maintained in GKI coordinates to make
	# the current position invariant with respect to changes in the
	# current WCS.  The current position is maintained in floating point
	# to minimize the accumulation of errors in relative moves and draws.

	cx = x
	cy = y

	# Convert to long integer metacode coords for clipping.

	mx = x
	my = y

	# Clip at either the viewport boundary or the edge of the device screen,
	# if clipping is "disabled".  Clipping is performed in NDC space rather
	# than world space because NDC space is simpler (mx1 < mx2, my1 < my2,
	# no log scaling), and because we need to clip at the device screen
	# boundary anyhow.  If the boundary is crossed the polyline is broken.
	# A line segment may lie entirely outside the viewport, entirely inside,
	# may cross from inside to outside, from outside to inside, or may
	# cross twice (cross two different boundaries).  The clipping algorithm
	# used (Harrington, 1983; Sutherland and Hodgman, 1974) clips at each
	# of the four boundaries in sequence, using the clipped point from the
	# previous iteration as input to the next.  It isn't simple but neither
	# is the problem.  The code is optimized for the usual inbounds case.
	# Clipped points are discarded.

	inbounds = (my <= my2 && my >= my1 && mx <= mx2 && mx >= mx1)

	if (inbounds && (last_point_inbounds || pl_pointmode == YES)) {
	    # Add point to polyline (the fast way).
	    pl[op] = mx
	    op = op + 1
	    pl[op] = my
	    op = op + 1

	} else if (pl_pointmode == NO)  {
	    if (last_point_inbounds) {
		# Update coords of last point drawn (necessary since we did
		# not use the clipping code for inbounds points).
		do i = 1, 4 {
		    xs[i] = pl[op-2]
		    ys[i] = pl[op-1]
		}
	    }
	    call gpl_clipl (DRAW, mx, my)
	}

	last_point_inbounds = inbounds

	# Break long polylines to avoid overflowing the polyline output
	# buffer.  The output buffer contains two cells for each output
	# point (x,y pair).  There must be space for at least two points
	# (four cells) left in the buffer, since a single clip operation
	# can add up to two points to the polyline.  OP points to the next
	# available cell.

	if (op > LEN_PLBUF - 2)
	    call gpl_flush()
end


# GPL_CLIPL -- Clip at left boundary.

procedure gpl_clipl (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
int	newpen
include	"gpl.com"

begin
	# Does line cross boundary?
	if ((mx >= mx1 && xs[1] < mx1) || (mx <= mx1 && xs[1] > mx1)) {
	    if (mx >= mx1)
		newpen = MOVE
	    else
		newpen = pen
	    call gpl_clipr (newpen, mx1,
		(my - ys[1]) * (mx1 - mx) / (mx - xs[1]) + my)
	}

	xs[1] = mx
	ys[1] = my

	if (mx >= mx1)
	    call gpl_clipr (pen, mx, my)
end


# GPL_CLIPR -- Clip at right boundary.

procedure gpl_clipr (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
int	newpen
include	"gpl.com"

begin
	# Does line cross boundary?
	if ((mx <= mx2 && xs[2] > mx2) || (mx >= mx2 && xs[2] < mx2)) {
	    if (mx <= mx2)
		newpen = MOVE
	    else
		newpen = pen
	    call gpl_clipb (newpen, mx2,
		(my - ys[2]) * (mx2 - mx) / (mx - xs[2]) + my)
	}

	xs[2] = mx
	ys[2] = my

	if (mx <= mx2)
	    call gpl_clipb (pen, mx, my)
end


# GPL_CLIPB -- Clip at bottom boundary.

procedure gpl_clipb (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
int	newpen
include	"gpl.com"

begin
	# Does line cross boundary?
	if ((my >= my1 && ys[3] < my1) || (my <= my1 && ys[3] > my1)) {
	    if (my >= my1)
		newpen = MOVE
	    else
		newpen = pen
	    call gpl_clipt (newpen, 
		(mx - xs[3]) * (my1 - my) / (my - ys[3]) + mx, my1)
	}

	xs[3] = mx
	ys[3] = my

	if (my >= my1)
	    call gpl_clipt (pen, mx, my)
end


# GPL_CLIPT -- Clip at top boundary and put the final clipped point(s) in
# the output polyline.  Note that a "move" at this level does not affect
# the current position (cx,cy), since the vector endpoints have been clipped
# and the current position vector follows the unclipped vector points input
# by the user.

procedure gpl_clipt (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
include	"gpl.com"

begin
	# Does line cross boundary?
	if ((my <= my2 && ys[4] > my2) || (my >= my2 && ys[4] < my2)) {
	    if (my <= my2 || pen == MOVE)
		call gpl_flush()
	    pl[op] = (mx - xs[4]) * (my2 - my) / (my - ys[4]) + mx
	    op = op + 1
	    pl[op] = my2
	    op = op + 1
	}

	xs[4] = mx
	ys[4] = my

	if (my <= my2) {
	    if (pen == MOVE)
		call gpl_flush()
	    pl[op] = mx
	    op = op + 1
	    pl[op] = my
	    op = op + 1
	}
end

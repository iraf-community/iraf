include <gset.h>
include <math.h>
include "wcslab.h"
include "wcs_desc.h"


# WL_GRID -- Put the grid lines/tick marks on the plot.
#
# Description
#  Based on previously determined parameters., draw the grid lines and/or
#  tick marks onto the graph.  While in the process of doing this, create
#  a list of possible label points for use by the label_grid routine.

procedure wl_grid (wd)

pointer wd		# I: the WCSLAB descriptor

double	current, tmp_begin, tmp_end, tmp_minor_interval
int	old_type, old_n_labels, min_counter
int	gstati()

begin
	# Initialize the label counter.
	WL_N_LABELS(wd) = 0

	# Remember what line type is currently active.
	old_type = gstati (WL_GP(wd), G_PLTYPE)

	# Determine integer range for axis 1.
	tmp_minor_interval = WL_MAJOR_INTERVAL(wd,AXIS1) / 
	    double (WL_MINOR_INTERVAL(wd,AXIS1))

	# If near-polar, the lines should go all the way to the poles.
	if (WL_GRAPH_TYPE(wd) == NEAR_POLAR)
	    if (abs (WL_BEGIN(wd,AXIS2)) < abs (WL_END(wd,AXIS2))) {
	        tmp_begin = WL_BEGIN(wd,AXIS2)
	        tmp_end = NORTH_POLE_LATITUDE
	    } else {
	        tmp_begin = SOUTH_POLE_LATITUDE
	        tmp_end = WL_END(wd,AXIS2)
	    }
	else {
	    tmp_begin = WL_BEGIN(wd,AXIS2)
	    tmp_end = WL_END(wd,AXIS2)
	}

	# Plot lines of constant value in axis 1.
	current = WL_BEGIN(wd,AXIS1)
	min_counter = 0
	repeat {

	    if  (mod (min_counter, WL_MINOR_INTERVAL(wd,AXIS1)) == 0) {
	        call gseti  (WL_GP(wd), G_PLTYPE, WL_MAJ_LINE_TYPE(wd))
	        call wl_graph_constant_axis1 (wd, current, tmp_begin, tmp_end, 
	            WL_MAJ_GRIDON(wd), WL_LABON(wd), WL_MAJ_TICK_SIZE(wd))
	    } else {
	        call gseti  (WL_GP(wd), G_PLTYPE, WL_MIN_LINE_TYPE(wd))
	        call wl_graph_constant_axis1 (wd, current, tmp_begin, tmp_end,
	            WL_MIN_GRIDON(wd), NO, WL_MIN_TICK_SIZE(wd))
	    }

	    min_counter = min_counter + 1
	    current = WL_BEGIN(wd,AXIS1) + tmp_minor_interval * min_counter

	} until  (real (current) > real (WL_END(wd,AXIS1)))

	# Determine the interval range for the second axis.
	tmp_minor_interval = WL_MAJOR_INTERVAL(wd,AXIS2) / 
	    double (WL_MINOR_INTERVAL(wd,AXIS2))

	# Plot lines of constant value in axis 2.
	if  (WL_END(wd,AXIS2) < WL_BEGIN(wd,AXIS2)) {
	    current = WL_END(wd,AXIS2)
	    tmp_minor_interval = -tmp_minor_interval
	    tmp_end = WL_BEGIN(wd,AXIS2)
	} else {
	    current = WL_BEGIN(wd,AXIS2)
	    tmp_end = WL_END(wd,AXIS2)
	}

	min_counter = 0
	tmp_begin = current 
	repeat {
	    if  (mod (min_counter, WL_MINOR_INTERVAL(wd,AXIS2)) == 0) {

	        call gseti  (WL_GP(wd), G_PLTYPE, WL_MAJ_LINE_TYPE(wd))
	        old_n_labels = WL_N_LABELS(wd)
	        call wl_graph_constant_axis2 (wd, current, WL_BEGIN(wd,AXIS1), 
	            WL_END(wd,AXIS1), WL_MAJ_GRIDON(wd), WL_LABON(wd), 
	            WL_MAJ_TICK_SIZE(wd))

	        # If this is a polar or near_polar plot, the latitudes
		# should be placed near the line, not where it crosses the
		# window boundary.

	        if  (WL_GRAPH_TYPE(wd) == POLAR &&
	            (WL_MAJ_GRIDON(wd) == YES) && (WL_LABON(wd) == YES)) {
	    	    WL_N_LABELS(wd) = old_n_labels + 1
	    	    call wl_w2ld (WL_WLCT(wd), WL_AXIS_FLIP(wd), 
	                WL_POLAR_LABEL_POSITION(wd), current,
	                WL_LABEL_POSITION(wd,WL_N_LABELS(wd),X_DIM),
	                WL_LABEL_POSITION(wd,WL_N_LABELS(wd),Y_DIM), 1)
	    	    WL_LABEL_VALUE(wd,WL_N_LABELS(wd)) = current
	    	    WL_LABEL_AXIS(wd,WL_N_LABELS(wd)) = AXIS2
	        }

	    } else {
	        call gseti  (WL_GP(wd), G_PLTYPE, WL_MIN_LINE_TYPE(wd))
	        call wl_graph_constant_axis2 (wd, current, WL_BEGIN(wd,AXIS1),
		    WL_END(wd,AXIS1), WL_MIN_GRIDON(wd), NO,
		    WL_MIN_TICK_SIZE(wd)) 
	    }

	    # Increment and continue
	    min_counter = min_counter + 1
	    current = tmp_begin + tmp_minor_interval * min_counter

	} until  (real (current) > real (tmp_end))

	# Set the line type back to the way it was.
	call gseti (WL_GP(wd), G_PLTYPE, old_type)
end


# WL_GRAPH_CONSTANT_AXIS1 - Graph lines of constant X-axis values.
#
# Description
#  Because projections are rarely linear, the basic GIO interface to draw
#  lines cannot be used.  Instead, this routine handles the line drawing.
#  Also, possible label points are found and added to a label list array.
#
# CLUDGE! Finding labels here is WRONG.  Ideally, crossing points (where the
# line crosses a screen boundary) should be determined analytically.  However,
# the MWCS interface lacks the required "cross-transformations".  It can
# still be done, but requires a total bypassing of MWCS.  Instead, this
# simplistic approach is used.

procedure wl_graph_constant_axis1 (wd, x, ymin, ymax, gridon, label, tick_size)

pointer wd          	# I: the WCSLAB descriptor
double  x               # I: X value to hold constant
double  ymin, ymax      # I: Y values to vary between
int	gridon          # I: true if gridding is on
int     label           # I: true if the points should be labelled
real    tick_size       # I: size of tick marks

bool	done
double	lastx, lasty, lx, ly, y, yinc
real	rlx, rly

begin
	# Determine the scale at which Y should be incremented.
	yinc =  (ymax - ymin) / WL_LINE_SEGMENTS(wd)

	# Now graph the line segments.
	y = ymin
	call wl_w2ld (WL_WLCT(wd), WL_AXIS_FLIP(wd), x, y, lastx, lasty, 1)

	rlx = lastx
	rly = lasty
	call gamove (WL_GP(wd), rlx, rly)

	repeat {
	    call wl_w2ld (WL_WLCT(wd), WL_AXIS_FLIP(wd), x, y, lx, ly, 1)
	    call wl_point_to_label (wd, lastx, lasty, lx, ly, AXIS1, x, gridon,
	        label, tick_size)
	    if (gridon == YES) {
		rlx = lx
		rly = ly
	        call gadraw (WL_GP(wd), rlx, rly)
	    }
	    if (yinc < 0.)
	        done = y < ymax
	    else
	        done = y > ymax
	    y = y + yinc
	    lastx = lx
	    lasty = ly
	} until  (done)
end


# WL_GRAPH_CONSTANT_AXIS2 -- Graph lines of constant Y-axis values.
#
# Description
#  Because projections are rarely linear, the basic GIO interface to draw
#  lines cannot be used.  Instead, this routine handles the line drawing.
#  Also, possible label points are found and added to an label list array.
#
# CLUDGE! Finding labels here is WRONG.  Ideally, crossing points (where the
# line crosses a screen boundary) should be determined analytically.  However,
# the MWCS interface lacks the required "cross-transformations".  It can
# still be done, but requires a total bypassing of MWCS.  Instead, this
# simplistic approach is used.

procedure wl_graph_constant_axis2 (wd, y, xmin, xmax, gridon, label, tick_size)

pointer wd              # I: the WCSLAB descriptor
double  y               # I: Y value to hold constant
double  xmin, xmax      # I: X values to vary between
int	gridon          # I: true if gridding is on
int	label           # I: true if points should be labelled
real    tick_size       # I: tick mark size

bool	done
double	lx, ly, lastx, lasty, x, xinc
real	rlx, rly

begin
	# Determine the scale at which X should be incremented.
	xinc =  (xmax - xmin) / WL_LINE_SEGMENTS(wd)

	# Now graph the line segments.
	x = xmin
	call wl_w2ld (WL_WLCT(wd), WL_AXIS_FLIP(wd), x, y, lastx, lasty, 1)

	rlx = lastx
	rly = lasty
	call gamove (WL_GP(wd), rlx, rly)

	repeat {
	    call wl_w2ld (WL_WLCT(wd), WL_AXIS_FLIP(wd), x, y, lx, ly, 1)
	    call wl_point_to_label (wd, lastx, lasty, lx, ly, AXIS2, y, gridon,
	        label, tick_size)
	    if  (gridon == YES) {
		rlx = lx
		rly = ly
	        call gadraw (WL_GP(wd), rlx, rly)
	    }
	    if  (xinc < 0.)
	        done = x < xmax
	    else
		done = x > xmax
	    lastx = lx
	    lasty = ly
	    x = x + xinc
	} until  (done)
end


# Define the inside and outside of the window.

define OUT (($1<=WL_SCREEN_BOUNDARY(wd,LEFT))||($1>=WL_SCREEN_BOUNDARY(wd,RIGHT))||($2<=WL_SCREEN_BOUNDARY(wd,BOTTOM))||($2>=WL_SCREEN_BOUNDARY(wd,TOP)))

define IN (($1>WL_SCREEN_BOUNDARY(wd,LEFT))&&($1<WL_SCREEN_BOUNDARY(wd,RIGHT))&&($2>WL_SCREEN_BOUNDARY(wd,BOTTOM))&&($2<WL_SCREEN_BOUNDARY(wd,TOP)))


# WL_POINT_TO_LABEL - Record a points position along a window boundary.
#
# Description
#  Since the MWCS interface lacks "cross-transformations", i.e. If given
#  RA and and X axis location, find DEC and Y axis, we need a different
#  method of determining when lines of constant Axis 1/Axis 2 cross
#  the window boundary.  Since each line is drawn by small increments, each
#  increment is watched to see if a window boundary has been crossed.  This
#  is what this routine does:  Confirms that a boundary has been crossed,
#  records this position and label value.  Tick marks are also drawn here
#  because all the necessary information is known at this point.
#
# NOTE: THIS WAY IS A CLUDGE !  A more formal method of finding
# cross-transformations is needed- most likely an iterative method.  This
# way was just "convenient at the time".

procedure wl_point_to_label (wd, x1, y1, x2, y2, axis, axis_value, gridon, 
	label, tick_size)

pointer wd                  # I: the WCSLAB descriptor
double  x1, y1, x2, y2      # I: the two possible points to label
int     axis                # I: which axis are we dealing with ?
double  axis_value          # I: the value of the axis at this point
int	gridon              # I: true if gridding is on
int	label               # I: true if this point should have a label
real    tick_size           # I: size of the tick mark

double	nx, ny, tick_x, tick_y
double	wl_vector_angle()

begin
	# Determine whether the two points straddle a window boundary. If they 
	# do, then this is the point to label.
	if  (OUT (x1, y1) && IN (x2, y2)) {

	    call wl_axis_on_line (x1, y1, x2, y2, WL_SCREEN_BOUNDARY(wd,1),
	        nx, ny)

	    if  (gridon == NO) {
	        call wl_mark_tick (WL_GP(wd), WL_NDC_WCS(wd), tick_size, 
	            WL_TICK_IN(wd), x1, y1, x2, y2, nx, ny, tick_x, tick_y)
	        if (WL_TICK_IN(wd) != WL_LABOUT(wd)) {
	            nx = tick_x
	            ny = tick_y
	        }
	    } 

	    if  ((label == YES) && WL_N_LABELS(wd) < MAX_LABEL_POINTS) {
	        WL_N_LABELS(wd) = WL_N_LABELS(wd) + 1
	        WL_LABEL_POSITION(wd,WL_N_LABELS(wd),AXIS1) = nx
	        WL_LABEL_POSITION(wd,WL_N_LABELS(wd),AXIS2) = ny
	        WL_LABEL_VALUE(wd,WL_N_LABELS(wd)) = axis_value
	        WL_LABEL_AXIS(wd,WL_N_LABELS(wd)) = axis
	    	WL_LABEL_ANGLE(wd,WL_N_LABELS(wd)) =
		    wl_vector_angle (WL_GP(wd), x1, y1, x2, y2)
	    }
	}

	if  (IN (x1, y1) && OUT (x2, y2)) {

	    call wl_axis_on_line (x2, y2, x1, y1, WL_SCREEN_BOUNDARY(wd,1), 
	        nx, ny)

	    if  (gridon == NO) {
	        call wl_mark_tick (WL_GP(wd), WL_NDC_WCS(wd), tick_size, 
	            WL_TICK_IN(wd), x2, y2, x1, y1, nx, ny, tick_x, tick_y)
	        if (WL_TICK_IN(wd) != WL_LABOUT(wd)) {
	      	    nx = tick_x
	      	    ny = tick_y
	        }
	    }

	    if  ((label == YES) && WL_N_LABELS(wd) < MAX_LABEL_POINTS) {
		WL_N_LABELS(wd) = WL_N_LABELS(wd) + 1
		WL_LABEL_POSITION(wd,WL_N_LABELS(wd),AXIS1) = nx
		WL_LABEL_POSITION(wd,WL_N_LABELS(wd),AXIS2) = ny
		WL_LABEL_VALUE(wd,WL_N_LABELS(wd)) = axis_value
		WL_LABEL_AXIS(wd,WL_N_LABELS(wd)) = axis
		WL_LABEL_ANGLE(wd,WL_N_LABELS(wd)) =
		    wl_vector_angle (WL_GP(wd), x1, y1, x2, y2)
	    }
	}

end


# WL_MARK_TICK - Draw the tick mark at the point.
#
# Description
#  Draw a tick mark rooted at (sx,sy), whose direction is defined by
#  the vector (x0,y0) to (x1,y1). The other end of the tick mark is
# returned in (tick_x,tick_y).

procedure wl_mark_tick (gp, wcs, tick_size, in, x0, y0, x1, y1, sx, sy,
	tick_x, tick_y)

pointer gp              # I: the graphics pointer
int     wcs             # I: the WCS to use to draw the tick marks
real    tick_size       # I: size of the tick mark
int	in              # I: true if ticks should be into the graph
double  x0, y0, x1, y1  # I: the points defining the tick direction
double	sx, sy		# I: the root point of the tick mark
double  tick_x, tick_y  # O: the end point of the tick mark

int	old_line, old_wcs
real	dx, dy, t, ndc_x0, ndc_y0, ndc_x1, ndc_y1, ndc_x2, ndc_y2
real	ndc_sx, ndc_sy
int	gstati()
real	wl_distancer()

begin
	# Change graphics coordinates to NDC.
	old_wcs = gstati (gp, G_WCS)
	old_line = gstati (gp, G_PLTYPE)
	call gseti (gp, G_WCS, wcs)
	call gseti (gp, G_PLTYPE, GL_SOLID)

	# Convert the points to NDC coordinates.
	ndc_x2 = real (sx)
	ndc_y2 = real (sy)
	call gctran (gp, ndc_x2, ndc_y2, ndc_sx, ndc_sy, old_wcs, wcs)
	ndc_x2 = real (x0)
	ndc_y2 = real (y0)
	call gctran (gp, ndc_x2, ndc_y2, ndc_x0, ndc_y0, old_wcs, wcs)
	ndc_x2 = real (x1)
	ndc_y2 = real (y1)
	call gctran (gp, ndc_x2, ndc_y2, ndc_x1, ndc_y1, old_wcs, wcs)

	# Determine the parameterized line parameters.
	dx = ndc_x1 - ndc_x0
	dy = ndc_y1 - ndc_y0

	# Determine how large in "time" the tick mark is.
	t = tick_size / wl_distancer (ndc_x0, ndc_y0, ndc_x1, ndc_y1)

	# If tick marks are to point out of the graph, reverse the sign of t.
	# Also need to turn clipping off for the ticks appear.
	if (in == NO) {
	  t = -t
	  call gseti (gp, G_CLIP, NO)
	}

	# Determine the end point of the tick mark.
	ndc_x2 = t * dx + ndc_sx
	ndc_y2 = t * dy + ndc_sy

	# Now draw the tick mark.
	call gamove (gp, ndc_sx, ndc_sy)
	call gadraw (gp, ndc_x2, ndc_y2)

	# Restore clipping if necessary.
	if (in == NO)
	  call gseti (gp, G_CLIP, YES)

	# Restore previous settings.
	call gseti (gp, G_WCS, old_wcs)
	call gseti (gp, G_PLTYPE, old_line)

	# Transform the end of the tick mark.
	call gctran (gp, ndc_x2, ndc_y2, dx, dy, wcs, old_wcs)
	tick_x = double (dx)
	tick_y = double (dy)
end


# WL_VECTOR_ANGLE -- Return the angle represented by the given vector.
#
# Returns
#   The angle of the given vector.

double procedure wl_vector_angle (gp, x1, y1, x2, y2)

pointer gp                  # I: the graphics descriptor
double  x1, y1, x2, y2      # I: the end points of the vector

double	dangle
real	angle, delx, dely, ndc_x1, ndc_x2, ndc_y1, ndc_y2
bool	fp_equalr()
int	gstati()

begin
	# Translate the input points to NDC coordinates.
	ndc_x1 = real (x1)
	ndc_x2 = real (x2)
	ndc_y1 = real (y1)
	ndc_y2 = real (y2)
	call gctran (gp, ndc_x1, ndc_y1, ndc_x1, ndc_y1, gstati (gp, G_WCS), 
	    NDC_WCS)
	call gctran (gp, ndc_x2, ndc_y2, ndc_x2, ndc_y2, gstati (gp, G_WCS), 
	    NDC_WCS)

	dely = ndc_y2 - ndc_y1
	delx = ndc_x2 - ndc_x1
	if (fp_equalr (delx, 0.) && fp_equalr (dely, 0.))
	    angle = 0.0
	else
	    angle = RADTODEG (atan2 (dely, delx))
	dangle = angle

	return (dangle)
end

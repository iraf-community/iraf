include <gset.h>
include <mach.h>
include <math.h>
include <math/curfit.h>
include "wcslab.h"
include "wcs_desc.h"

# WL_SETUP -- Determine all the basic characteristics of the plot.
#
# Description
#  Determine basic characteristics of the plot at hand.  This involved
#  "discovering" what part of the world system covers the screen, the
#  orientation of the world to logical systems, what type of graph will
#  be produced, etc.  Many of the parameters determined here can be 
#  over-ridden by user-specified values.

procedure wl_setup (wd)

pointer wd                       # I: the WCSLAB descriptor

bool	north
double	array[N_EDGES,N_DIM], max_value[N_DIM], min_value[N_DIM]
double	range[N_DIM], pole_position[N_DIM], view_edge[N_EDGES,N_DIM]
double	wl_coord_rotation()
pointer mw_sctran()
string	logtran "logical"
string	wrldtran "world"

begin
	# Calculate the transformations from the Logical (pixel space) system
	# to the World (possibly anything) system and back.
	WL_LWCT(wd) = mw_sctran (WL_MW(wd), logtran, wrldtran, AXIS)
	WL_WLCT(wd) = mw_sctran (WL_MW(wd), wrldtran, logtran, AXIS)

	# Indicate whether the center of the transformation is north.
	if (WL_SYSTEM_TYPE(wd) == RA_DEC)
	    north = (WL_WORLD_CENTER(wd,LATITUDE) > 0.0D0)

	# Determine the poles position.
	if (WL_SYSTEM_TYPE(wd) == RA_DEC)
	    call wl_pole_position (WL_WLCT(wd), WL_AXIS_FLIP(wd), 
	        WL_WORLD_CENTER(wd,LONGITUDE), north, WL_SYSTEM_TYPE(wd),
		pole_position)

	# Determine graph type based on the system type.
	call wl_determine_graph_type (WL_SYSTEM_TYPE(wd), pole_position,
	    WL_SCREEN_BOUNDARY(wd,1), WL_GRAPH_TYPE(wd))

	# Now find the extent of the WCS the window views, by constructing
	# x,y vectors containing evenly spaced points around the edges of
	# the viewing window.

	call wl_construct_edge_vectors (WL_SCREEN_BOUNDARY(wd,1),
	    view_edge[1,X_DIM], view_edge[1,Y_DIM], N_EDGES)
	
	# Find the range of the axes over the graphics viewport.
	call wl_l2wd (WL_LWCT(wd), WL_AXIS_FLIP(wd), view_edge[1,X_DIM], 
	     view_edge[1,Y_DIM], array[1,AXIS1], array[1,AXIS2], N_EDGES)
	call alimd (array[1,AXIS1], N_EDGES, min_value[AXIS1], max_value[AXIS1])
	call alimd (array[1,AXIS2], N_EDGES, min_value[AXIS2], max_value[AXIS2])
	range[AXIS1] = abs (max_value[AXIS1] - min_value[AXIS1])
	range[AXIS2] = abs (max_value[AXIS2] - min_value[AXIS2])

	# The above isn't good enough for the sky projections.  Deal with those.
	if (WL_SYSTEM_TYPE(wd) == RA_DEC)
	    call wl_sky_extrema (wd, array[1,AXIS1], N_EDGES, pole_position,
	        north, min_value[AXIS1], max_value[AXIS1], range[AXIS1],
		min_value[AXIS2], max_value[AXIS2], range[AXIS2])

	# Determine the rotation between the systems.
	WL_ROTA(wd) = wl_coord_rotation (WL_WLCT(wd), WL_AXIS_FLIP(wd),
	    WL_WORLD_CENTER(wd,AXIS1), max_value[AXIS2],
	    WL_WORLD_CENTER(wd,AXIS1), min_value[AXIS2])

	# Round the intervals.  This is done to make the labelling "nice" and
	# to smooth edge effects.
	if (IS_INDEFD (WL_MAJOR_INTERVAL(wd,AXIS1)) ||
	    IS_INDEFD (WL_BEGIN(wd,AXIS1)) || IS_INDEFD (WL_END(wd,AXIS1)))
	    call wl_round_axis (wd, AXIS1, min_value[AXIS1], max_value[AXIS1],
	        range[AXIS1])

	if (IS_INDEFD (WL_MAJOR_INTERVAL(wd,AXIS2)) ||
	    IS_INDEFD (WL_BEGIN(wd,AXIS2)) || IS_INDEFD (WL_END(wd,AXIS2)))
	    call wl_round_axis (wd, AXIS2, min_value[AXIS2], max_value[AXIS2],
	        range[AXIS2])
end


# WL_POLE_POSITION -- Determine logical coordinates of a pole.
#
# Description
#  Calculate the pole's position in the Logical system.
#
# Bugs
#  Can only deal with Right Ascension/Declination.

procedure wl_pole_position (wlct, flip, longitude, north, system_type, 
	pole_position)

pointer wlct                  # I: the world-to-logical transformation
int	flip                  # I: true if the axes are transposed
double  longitude             # I: the longitude to determine latitude
bool    north                 # I: true if the pole is in the north
int     system_type           # I: type of system being examined
double  pole_position[N_DIM]  # O: the pole's logical coordinates

double	sgn

begin
	switch (system_type) {

	# For Right Ascension/Declination, the pole is at any longitude but
	# at only 90 degrees (north) or -90 degrees (south) latitude.
	case RA_DEC:
	    if  (north)
		sgn = NORTH_POLE_LATITUDE
	    else
		sgn = SOUTH_POLE_LATITUDE
	    call wl_w2ld (wlct, flip, longitude, sgn, pole_position[X_DIM],
                pole_position[Y_DIM], 1)
	}

	# Sanity check on the pole position.  It is very likely that there is
	# no valid position in pixel space for the pole.  This is checked for
	# by looking for extremely large numbers.
	if  (abs (pole_position[X_DIM]) > abs (double (MAX_INT)))
	    pole_position[X_DIM] = real (MAX_INT)
	if  (abs (pole_position[Y_DIM]) > abs (double (MAX_INT)))
	    pole_position[Y_DIM] = real (MAX_INT)
end


# How close can the pole be to the center of the screen to be near-polar.
define	HOW_CLOSE	3.

# WL_DETERMINE_GRAPH_TYPE -- Determine the actual graph type.

procedure wl_determine_graph_type (system_type, pole_position,
	screen_boundary, graph_type)

int	system_type               # I: the type of WCS being dealt with
double	pole_position[N_DIM]      # I: the location of the pole
double	screen_boundary[N_SIDES]  # I: the edges of the display
int	graph_type                # O: the graph type

double	max_dist, pole_dist, xcen, ycen

begin
	# Determine graph type based on axis type.
	switch (system_type) {

	# If the pole is on the graph then force a graph_type of polar.
	case RA_DEC:

	    xcen = (screen_boundary[LEFT] + screen_boundary[RIGHT]) / 2.
	    ycen =  (screen_boundary[BOTTOM] + screen_boundary[TOP]) / 2.
	    max_dist = min ((screen_boundary[LEFT] - xcen) ** 2,
	        (screen_boundary[TOP] - ycen)**2)
	    pole_dist = (pole_position[X_DIM] - xcen) ** 2 +
	        (pole_position[Y_DIM] - ycen) ** 2
	
	    # Check to see whether the graph is "polar", "near_polar"
	    # or "normal".  If the pole lies within middle part of the
	    # viewport, then the graph is "polar". If the pole is within
	    # a certain maximum distance then it is "near_polar".
	    # Otherwise it is normal.

	    switch (graph_type) {
	    case NORMAL:
		# do nothing
	    case POLAR:
		# do nothing
	    case NEAR_POLAR:
		# do nothing
	    default:
	        if (pole_dist < max_dist)
	            graph_type = POLAR
	        else if  (pole_dist < HOW_CLOSE * max_dist)
	            graph_type = NEAR_POLAR
	        else
	            graph_type = NORMAL
	    }

	# For all other cases, explicitely set this to normal.
	default:
	    graph_type = NORMAL
	}
end


# WL_CONSTRUCT_EDGE_VECTORS -- Construct vectors of values along window's edge.
#
# Description
#  This routines filles two arrays, with the x-values and y-values of
#  evenly spaced points along the edges of the screen.  This is used to
#  make transformation of the logical edges into the world system
#  more convenient.

procedure wl_construct_edge_vectors (screen_boundary, x, y, vector_size)

double	screen_boundary[N_SIDES]       # I: the side values
double	x[vector_size], y[vector_size] # O: the edge vector points
int	vector_size                    # I: the number of edge vector points

double	current, interval
int	i, left_over, offset1, offset2, side_length

begin
	# Divide the vectors into equal amounts for each side.
	side_length = vector_size / N_SIDES
	left_over = mod (vector_size, N_SIDES)

	# Calculate the horizontal components.
	interval =  (screen_boundary[RIGHT] - screen_boundary[LEFT]) /
	    side_length
	current = screen_boundary[LEFT]
	offset1 = side_length
	for  (i = 1; i <= side_length; i = i + 1) {
	    x[i] = current + interval
	    y[i] = screen_boundary[BOTTOM]
	    x[i+offset1] = current
	    y[i+offset1] = screen_boundary[TOP]
	    current = current + interval 
	}

	# Calculate the verticle components.
	interval = (screen_boundary[TOP] - screen_boundary[BOTTOM]) /
	    side_length
	current = screen_boundary[BOTTOM]
	offset1 = 2 * side_length
	offset2 = 3 * side_length
	for  (i = 1; i <= side_length; i = i + 1) {
	    x[i+offset1] = screen_boundary[LEFT]
	    y[i+offset1] = current
	    x[i+offset2] = screen_boundary[RIGHT]
	    y[i+offset2] = current + interval
	    current = current + interval 
	}

	# Fill in the left over with a single point.
	offset1 = 4 * side_length
	for  (i = 1; i <= left_over; i = i + 1) {
	    x[i+offset1] = screen_boundary[LEFT]
	    y[i+offset1] = screen_boundary[BOTTOM]
	}

end


# WL_SKY_EXTREMA -- Determine what range the view window covers in the sky.
# This routine is only called if the WCS RA,DEC.
#
# Description
#   Because of the different graph types and the fact that axis 1 usually
#   wraps, more work needs to be done to determine what part of the sky
#   is covered by the viewing window.

procedure wl_sky_extrema (wd, ax1_array, n_points, pole_position, north,
	ax1min, ax1max, ax1ran, ax2min, ax2max, ax2ran)

pointer	wd                         # I: the WCSLAB descriptor
double  ax1_array[n_points]        # I: the axis 1 edge vector
int     n_points                   # I: the length of the edge vector
double  pole_position[N_DIM]       # I: the pole position
bool    north                      # I: is the pole in the north ?
double  ax1min, ax1max, ax1ran     # I/O: the minimum, maximum, range in axis 1
double  ax2min, ax2max, ax2ran     # I/O: the minimum, maximum, range in axis 2

bool	is_pole
double	nx, ny, xcen, ycen
int	wl_direction_from_axis1(), wl_find_side(), wl_opposite_side()

begin
	# Is the pole on the graph ?
	if ((pole_position[X_DIM] < WL_SCREEN_BOUNDARY(wd,LEFT))   ||
	    (pole_position[X_DIM] > WL_SCREEN_BOUNDARY(wd,RIGHT))  ||
	    (pole_position[Y_DIM] < WL_SCREEN_BOUNDARY(wd,BOTTOM)) ||
	    (pole_position[Y_DIM] > WL_SCREEN_BOUNDARY(wd,TOP)))
	    is_pole = false
	else
	    is_pole = true

	# If so adjust the RA and DEC ranges appropriately.
	if (is_pole) {

	    # Set the RA range.
	    ax1min = 0.0D0
	    ax1max = 359.9D0
	    ax1ran = 360.0D0

	    # Set the dec range.
	    if (north)
	        ax2max = NORTH_POLE_LATITUDE - ((NORTH_POLE_LATITUDE -
		    ax2min) * DISTANCE_TO_POLE )
	    else
	        ax2min = SOUTH_POLE_LATITUDE + ((NORTH_POLE_LATITUDE +
		    ax2max) * DISTANCE_TO_POLE)
	    ax2ran = abs (ax2max - ax2min)

	    # Mark the pole.
	    call gmark (WL_GP(wd), real (pole_position[X_DIM]),
		real (pole_position[Y_DIM]), POLE_MARK_SHAPE, POLE_MARK_SIZE,
		POLE_MARK_SIZE)

	} else {
	     # Only the RA range needs adjusting.
	     call wl_ra_range (ax1_array, n_points, ax1min, ax1max, ax1ran)
	}

	# Adjust the labelling characteristics appropritatley for various
	# types of graphs.

	if  (WL_GRAPH_TYPE(wd) == POLAR) {

	    # Determine which direction the axis 2's will be labeled on polar
	    # graphs.
	    if (IS_INDEFD (WL_POLAR_LABEL_POSITION(wd))) {
	        call wl_get_axis2_label_direction (WL_LWCT(wd),
		    WL_AXIS_FLIP(wd), pole_position, WL_SCREEN_BOUNDARY(wd,1),
		    WL_POLAR_LABEL_POSITION(wd), WL_BAD_LABEL_SIDE(wd))
	    } else {
	        WL_BAD_LABEL_SIDE(wd) = wl_direction_from_axis1 (WL_WLCT(wd),
	            WL_AXIS_FLIP(wd), pole_position, north,
		    WL_POLAR_LABEL_POSITION(wd), WL_BEGIN(wd,AXIS2),
	            WL_END(wd,AXIS2), WL_SCREEN_BOUNDARY(wd,1))
		if (IS_INDEFI (WL_BAD_LABEL_SIDE(wd)))
		    WL_BAD_LABEL_SIDE(wd) = BOTTOM
	    }

	    # If the graph type is polar, then determine how to justify
	    # the labels.

	    if (IS_INDEFI (WL_POLAR_LABEL_DIRECTION(wd)))
	        WL_POLAR_LABEL_DIRECTION(wd) =
	            wl_opposite_side (WL_BAD_LABEL_SIDE(wd))

	# If the graph_type is near-polar, then handle the directions a bit
	# differently.
	} else if (WL_GRAPH_TYPE(wd) == NEAR_POLAR) {

	    # Find the side that the pole is on.  
	    xcen = (WL_SCREEN_BOUNDARY(wd,LEFT) +
	        WL_SCREEN_BOUNDARY(wd,RIGHT)) / 2.
	    ycen = (WL_SCREEN_BOUNDARY(wd,BOTTOM) +
	        WL_SCREEN_BOUNDARY(wd,TOP)) / 2.
	    call wl_axis_on_line (xcen, ycen, pole_position[X_DIM],
	        pole_position[Y_DIM], WL_SCREEN_BOUNDARY(wd,1), nx, ny)

	    if (IS_INDEFD(nx) || IS_INDEFD(ny)) {
		WL_BAD_LABEL_SIDE(wd) = BOTTOM
		WL_POLAR_LABEL_DIRECTION(wd) = LEFT
	    } else {
	        WL_BAD_LABEL_SIDE(wd) = wl_find_side (nx, ny,
	            WL_SCREEN_BOUNDARY(wd,1))
	        if (WL_BAD_LABEL_SIDE(wd) == LEFT || WL_BAD_LABEL_SIDE(wd) ==
	            RIGHT)
		    if (abs (ny - WL_SCREEN_BOUNDARY(wd,BOTTOM)) <
	                abs (ny - WL_SCREEN_BOUNDARY(wd,TOP)))
		        WL_POLAR_LABEL_DIRECTION(wd) = BOTTOM
		    else
		        WL_POLAR_LABEL_DIRECTION(wd) = TOP
	        else
		    if (abs (nx - WL_SCREEN_BOUNDARY(wd,LEFT)) <
	                abs (nx - WL_SCREEN_BOUNDARY(wd,RIGHT)))
		        WL_POLAR_LABEL_DIRECTION(wd) = LEFT
		    else
		        WL_POLAR_LABEL_DIRECTION(wd) = RIGHT
	    }

	}
end


# WL_COORD_ROTATION -- Determine "rotation" between the coordinate systems.
#
# Description
#  This routine takes the world-to-logical coordinate transformation and
#  two points in the world system which should define the positive verticle
#  axis in the world system.  These points are translated into the logical
#  system and the angle between the logical vector and its positive verticle
#  vector is calculated and returned.  The rotation angle is returned
#  in degrees and is always positive.

double	procedure wl_coord_rotation (wlct, flip, wx1, wy1, wx2, wy2)

pointer	wlct               # I: the world-to-logical transformation
int	flip               # I: true if the coordinates are transposed
double	wx1, wy1, wx2, wy2 # I: points in world space to figure rotation from

double	delx, dely, rota, x1, y1, x2, y2
bool	fp_equald()

begin
	# Transform the points to the logical system.
	call wl_w2ld (wlct, flip, wx1, wy1, x1, y1, 1)
	call wl_w2ld (wlct, flip, wx2, wy2, x2, y2, 1)

	# Determine the rotation.
	delx = x2 - x1
	dely = y2 - y1
	if (fp_equald (delx, 0.0D0) && fp_equald (dely, 0.0D0))
	    rota = 0.
	else
	    rota = RADTODEG (atan2 (dely, delx))

	if  (rota < 0.0D0)
	    rota = rota + FULL_CIRCLE

	return (rota)
end


# Define how many axis one should go for.

define RA_NUM_TRY         6
define DEC_NUM_TRY        6
define DEC_POLAR_NUM_TRY  4

# WL_ROUND_AXIS - Round values for the axis.

procedure wl_round_axis (wd, axis, minimum, maximum, range)

pointer wd                       # I: the WCSLAB descriptor
int     axis                     # I: the axis being worked on
double  minimum, maximum, range  # I: raw values to be rounded

int	num_try

begin
	# Depending on axis type, round the values.
	switch (WL_SYSTEM_TYPE(wd)) {
	case RA_DEC:
	    if (axis == LONGITUDE)
	        call wl_round_ra (minimum, maximum, range, RA_NUM_TRY,
	            WL_BEGIN(wd,LONGITUDE), WL_END(wd,LONGITUDE),
		    WL_MAJOR_INTERVAL(wd,LONGITUDE))
	    else {
	        if (WL_GRAPH_TYPE(wd) == POLAR)
	            num_try = DEC_POLAR_NUM_TRY
	        else
	            num_try = DEC_NUM_TRY
	        call wl_round_dec (minimum, maximum, range, num_try,
	            WL_BEGIN(wd,LATITUDE), WL_END(wd,LATITUDE), 
	            WL_MAJOR_INTERVAL(wd,LATITUDE))
	    }

	default:
	    call wl_generic_round (minimum, maximum, range, WL_BEGIN(wd,axis),
	        WL_END(wd,axis), WL_MAJOR_INTERVAL(wd,axis))
	}

end


# WL_GET_AXIS2_LABEL_DIRECTION -- Dertermine label direction for latitides.
#
# Description
#  Determine from which edge of the graph the axis 2 labels are to
#  appear.  This (in general) is the opposite edge from which the pole
#  is nearest to.  Move the pole to the closest edges, determine which
#  side it is, then chose the direction as the opposite.  Also determines
#  the Axis 1 at which the Axis 2 labels will appear.

procedure wl_get_axis2_label_direction (lwct, flip, pole_position, 
	screen_boundary, pole_label_position, bad_label_side)

pointer lwct                      # I: logical-to-world transformation
int	flip                      # I: true if the axis are transposed
double  pole_position[N_DIM]      # I: the position of the pole
double  screen_boundary[N_SIDES]  # I: the edges of the screen
double  pole_label_position       # O: the axis 1 that axis 2 labels should
	                          #    appear for polar|near-polar graphs
int     bad_label_side            # O: side not to place axis 1 labels

double	dif, tdif, dummy

begin
	# Determine which direction, up or down, the axis 2's will be labelled.
	dif = abs (screen_boundary[TOP] - pole_position[AXIS2])
	bad_label_side= TOP
	tdif = abs (screen_boundary[BOTTOM] - pole_position[AXIS2])
	if (tdif < dif) {
	  dif = tdif
	  bad_label_side = BOTTOM
	}

	# Determine at what value of Axis 1 the Axis 2 labels should appear.
	switch (bad_label_side) {
	case TOP: 
	    call wl_l2wd (lwct, flip, pole_position[AXIS1],
	        screen_boundary[BOTTOM], pole_label_position, dummy, 1)
	case BOTTOM: 
	    call wl_l2wd (lwct, flip, pole_position[AXIS1],
	        screen_boundary[TOP], pole_label_position, dummy, 1)
	case LEFT: 
	    call wl_l2wd (lwct, flip, screen_boundary[RIGHT],
	        pole_position[AXIS2], pole_label_position, dummy, 1)
	case RIGHT:  
	    call wl_l2wd (lwct, flip, screen_boundary[LEFT],
	        pole_position[AXIS2], pole_label_position, dummy, 1)
	}

end


# WL_DIRECTION_FROM_AXIS1 -- Determine axis 2 label direction from axis 1.
#
# Function Returns
#   This returns the side where Axis 1 should not be labelled.

int procedure wl_direction_from_axis1 (wlct, flip, pole_position, north,
	polar_label_position, lbegin, lend, screen_boundary)

pointer wlct                      # I: world-to-logical transformation
int	flip                      # I: true if the axes are transposed
double  pole_position[N_DIM]      # I: the pole position
bool    north                     # I: true if the pole is the north pole
double  polar_label_position      # I: the axis 1 where axis 2 will be 
	                          #    marked
double  lbegin                    # I: low end of axis 2
double  lend                      # I: high end of axis 2
double  screen_boundary[N_SIDES]  # I: the window boundary

double	nx, ny, cx, cy
int	wl_find_side()

begin
	# Determine the point in logical space where the axis 1 and the
	# minimum axis 2 meet.

	if (north)
	    call wl_w2ld (wlct, flip, polar_label_position, lbegin, nx, ny, 1)
	else
	    call wl_w2ld (wlct, flip, polar_label_position, lend, nx, ny, 1)

	# This line should cross a window boundary.  Find that point.

	call wl_axis_on_line (pole_position[X_DIM], pole_position[Y_DIM],
	    screen_boundary, nx, ny, cx, cy)

	# Get the side that the crossing point is. This is the axis 2 labelling
	# direction.

	if (IS_INDEFD(cx) || IS_INDEFD(cy))
	    return (INDEFI)
	else
	    return (wl_find_side (cx, cy, screen_boundary))
end


# WL_OPPOSITE_SIDE - Return the opposite of the given side.
#
# Returns
#   The opposite side of the specified side as follows:
#           RIGHT  -> LEFT
#           LEFT   -> RIGHT
#           TOP    -> BOTTOM
#           BOTTOM -> TOP

int procedure wl_opposite_side (side)

int	side		# I: the side to find the opposite of

int	new_side

begin
	switch (side) {
	case LEFT:
	    new_side = RIGHT
	case RIGHT:
	    new_side = LEFT
	case TOP:
	    new_side = BOTTOM
	case BOTTOM:
	    new_side = TOP
	}

	return (new_side)
end


# Define whether things are on the screen boundary or on them.

define IN (($1>=screen_boundary[LEFT])&&($1<=screen_boundary[RIGHT])&&($2>=screen_boundary[BOTTOM])&&($2<=screen_boundary[TOP]))


# WL_AXIS_ON_LINE - Determine intersection of line and a screen boundary.
#
# Description
#  Return the point where the line defined by the two input points 
#  crosses a screen boundary.  The boundary is choosen by determining
#  which one is between the two points.

procedure wl_axis_on_line (x0, y0, x1, y1, screen_boundary, nx, ny)

double	x0, y0, x1, y1            # I: random points in space
double	screen_boundary[N_SIDES]  # I: sides of the window
double	nx, ny                    # O: the closest point on a window boundary

double	x_val[N_SIDES], y_val[N_SIDES], tx0, ty0, tx1, ty1, w[2]
int	i
pointer	cvx, cvy
double	dcveval()

begin
	# Get the line parameters.
	x_val[1] = x0
	x_val[2] = x1
	y_val[1] = y0
	y_val[2] = y1

	iferr (call dcvinit (cvx, CHEBYSHEV, 2, min (x0, x1), max (x0, x1)))
	    cvx = NULL
	else {
	    call dcvfit (cvx, x_val, y_val, w, 2, WTS_UNIFORM, i)
	    if (i != OK)
	        call error (i, "wlaxie: Error solving on X")
	}

	iferr (call dcvinit (cvy, CHEBYSHEV, 2, min (y0, y1), max (y0, y1)))
	    cvy = NULL
	else {
	    call dcvfit (cvy, y_val, x_val, w, 2, WTS_UNIFORM, i)
	    if (i != OK)
	        call error (i, "wlaxie: Error solving on Y")
	}

	# Solve for each side.
	x_val[LEFT] = screen_boundary[LEFT]
	if (cvx == NULL)
	    y_val[LEFT] = screen_boundary[LEFT]
	else
	    y_val[LEFT] = dcveval (cvx, x_val[LEFT])

	x_val[RIGHT] = screen_boundary[RIGHT]
	if (cvx == NULL )
	    y_val[RIGHT] = screen_boundary[RIGHT]
	else
	    y_val[RIGHT] = dcveval (cvx, x_val[RIGHT])

	y_val[TOP] = screen_boundary[TOP]
	if (cvy == NULL)
	    x_val[TOP] = screen_boundary[TOP]
	else
	    x_val[TOP] = dcveval (cvy, y_val[TOP])

	y_val[BOTTOM] = screen_boundary[BOTTOM]
	if (cvy == NULL)
	    x_val[BOTTOM] = screen_boundary[BOTTOM]
	else
	    x_val[BOTTOM] = dcveval (cvy, y_val[BOTTOM])

	# Rearrange the input points to be in ascending order.
	if  (x0 < x1) {
	    tx0 = x0
	    tx1 = x1
	} else {
	    tx0 = x1
	    tx1 = x0
	}

	if  (y0 < y1) {
	    ty0 = y0
	    ty1 = y1
	} else {
	    ty0 = y1
	    ty1 = y0
	}

	# Now find which point is between the two given points and is within
	# the viewing area.
	# NOTE: Conversion to real for the check- if two points are so close
	# for double, any of them would serve as the correct answer.

	nx = INDEFD
	ny = INDEFD
	for  (i = 1; i <= N_SIDES; i = i + 1)
	    if  (real (tx0) <= real (x_val[i]) && 
	        real (x_val[i]) <= real (tx1)  &&
	        real (ty0) <= real (y_val[i])  && 
	        real (y_val[i]) <= real (ty1)  &&
	        IN (x_val[i], y_val[i]) ) {
	        nx = x_val[i]
	        ny = y_val[i]
	    }

	# Release the curve fit descriptors.
	if (cvx != NULL)
	    call dcvfree (cvx)
	if (cvy != NULL)
	    call dcvfree (cvy)
end


# WL_FIND_SIDE -- Return the side that the given point is lying on.
#
# Function Returns
#  Return the side, TOP, BOTTOM, LEFT, or RIGHT, that the specified
#  point is lying on.  One of the coordinates must be VERY CLOSE to one of 
#  the sides or INDEFI will be returned.

int procedure wl_find_side (x, y, screen_boundary)

double x, y                      # I: the point to inquire about
double screen_boundary[N_SIDES]  # I: the edges of the screen

double	dif, ndif
int	side

begin
	dif = abs (x - screen_boundary[LEFT])
	side = LEFT

	ndif = abs (x - screen_boundary[RIGHT])
	if  (ndif < dif) {
	  side = RIGHT
	  dif = ndif
	}
	
	ndif = abs (y - screen_boundary[BOTTOM])
	if  (ndif < dif) {
	  side = BOTTOM
	  dif = ndif
	}
	
	ndif = abs (y - screen_boundary[TOP])
	if  (ndif < dif)
	  side = TOP

	return (side)
end


# WL_RA_RANGE -- Determine the range in RA given a list of possible values.
#
# Description
#  Determine the largest range in RA from the provided list of values.
#  The problem here is that it is unknown which way the graph is oriented.
#  To simplify the problem, it is assume that the graph range does not extend
#  beyond a hemisphere and that all distances in RA is less than a hemisphere.
#  This assumption is needed to decide when the 0 hour is on the graph.

procedure wl_ra_range (ra, n_values, min, max, diff)

double	ra[ARB]		# I:   the possible RA values
int	n_values	# I:   the number of possible RA values
double	min		# I/O: the minimum RA
double	max		# I/O: the maximum RA
double	diff		# I/O: the difference between minimum and maximum

bool	wrap
int	i, j, n_diffs
pointer	sp, max_array, min_array, ran_array
int	wl_max_element_array()

begin
	call smark (sp)
	call salloc (max_array, n_values * n_values, TY_DOUBLE)
	call salloc (min_array, n_values * n_values, TY_DOUBLE)
	call salloc (ran_array, n_values * n_values, TY_DOUBLE)

	# Check whether the RA is wrapped or not.
	n_diffs = 0
	do i = 1, n_values {
	    if (ra[i] >= min && ra[i] <= max)
		next
	    n_diffs = n_diffs + 1
	}
	if (n_diffs > 0)
	    wrap = true
	else
	    wrap = false

	n_diffs = 0
	for  (i = 1; i <= n_values; i = i + 1) {
	    for  (j = i + 1; j <= n_values; j = j + 1) {
	        n_diffs = n_diffs + 1
	        call wl_getradif (ra[i], ra[j], Memd[min_array+n_diffs-1],
		    Memd[max_array+n_diffs-1], Memd[ran_array+n_diffs-1],
		    wrap)
	    }
	}

	i = wl_max_element_array (Memd[ran_array], n_diffs)
	min = Memd[min_array+i-1]
	max = Memd[max_array+i-1]
	diff = Memd[ran_array+i-1]

	call sfree (sp)
end


# WL_GETRADIFF -- Get differences in RA based on degrees.
#
# Description
#  This procedure determines, given two values in degrees, the minimum,
#  maximum, and difference of those values.  The assumption is that no
#  difference should be greater than half a circle.  Based on this assumption,
#  a difference is found and the minimum and maximum are determined.  The
#  maximum can be greater than 360 degrees.

procedure wl_getradif (val1, val2, min, max, diff, wrap)

double	val1, val2  # I: the RA values
double	min, max    # O: the min RA and max RA (possibly > 360.0)
double	diff        # O: the min, max difference
bool	wrap	    # I: is the ra wrapped ?

begin
	if (! wrap && (abs (val1 - val2) > HALF_CIRCLE))
	    if (val1 < val2) {
	        min = val2
	        max = val1 + FULL_CIRCLE
	    } else {
	        min = val1
	        max = val2 + FULL_CIRCLE
	    }
	else
	    if (val1 < val2) {
		min = val1
		max = val2
	    } else {
		min = val2
		max = val1
	    }
	diff = max - min
end


define	NRAGAP    26

# WL_ROUND_RA -- Modify the RA limits and calculate an interval to label.
#
# Description
#  The RA limits determine by just the extremes of the window ususally do
#  not fall on "reasonable" boundaries; i.e. essentially they are random
#  numbers.  However, for labelling purposes, it is nice to have grids and
#  tick marks for "rounded" numbers-  For RA, this means values close to
#  whole hours, minutes, or seconds.  For example, if the span across the
#  plot is a few hours, the marks and labels should represent simply whole
#  hours.  This routine determines new RA limits based on this and some 
#  interval to produce marks between the newly revised limits.

procedure wl_round_ra (longmin, longmax, longran, num_try, minimum, maximum, 
	major_interval)

double	longmin          # I: longitude minimum
double	longmax          # I: longitude maximum
double	longran          # I: longitude range
int	num_try          # I: the number of intervals to try for
double	minimum          # O: the minimum RA value (in degrees)
double	maximum          # O: the maximum RA value (in degrees)
double	major_interval   # O: the appropriate interval (in degrees) for the
	                 #    major line marks.

double  ragap[NRAGAP]
double	wl_check_arrayd(), wl_round_upd()
data	ragap / 1.0D-4, 2.0D-4, 5.0D-4,  1.0D-3,  2.0D-3,  5.0D-3,
	        0.01D0, 0.02D0, 0.05D0, 0.1D0,  0.2D0,  0.5D0,   1.0D0,
		2.0D0,   5.0D0, 10.0D0, 20.0D0, 30.0D0, 60.0D0, 120.0D0,
		300.0D0, 600.0D0,  1.2D3,  1.8D3,  3.6D3,  7.2D3 /


begin
	major_interval = wl_check_arrayd (DEGTOST (longran) / num_try, 
	    ragap, NRAGAP)
	minimum = STTODEG (wl_round_upd (DEGTOST (longmin), major_interval) - 
	    major_interval)
	maximum = STTODEG (wl_round_upd (DEGTOST (longmax), major_interval))
	major_interval = STTODEG (major_interval)
end


define	NDECGAP    28

# WL_ROUND_DEC -- Modify the DEC limits and calculate an interval to label.
#
# Description
#  The DEC limits determine by just the extremes of the window ususally do
#  not fall on "reasonable" boundaries; i.e. essentially they are random
#  numbers.  However, for labelling purposes, it is nice to have grids and
#  tick marks for "rounded" numbers-  For DEC, this means values close to
#  whole degrees, minutes, or seconds.  For example, if the span across the
#  plot is a few degrees, the marks and labels should represent simply whole
#  degrees.  This routine determines new DEC limits based on this and some 
#  interval to produce marks between the newly revised limits.

procedure wl_round_dec (latmin, latmax, latran, num_try, minimum, maximum, 
	major_interval)

double latmin              # I: the latitude minimum
double latmax              # I: the latitude maximum
double latran              # I: the latitude range
int    num_try             # I: number of intervals to try for
double minimum             # O: the DEC minimum
double maximum             # O: the DEC maximum
double major_interval      # O: the labelling interval to use for major lines

double  decgap[NDECGAP]
double	wl_check_arrayd(), wl_round_upd()
data	decgap / 1.0D-4, 2.0D-4,  5.0D-4, 1.0D-3, 2.0D-3,  5.0D-3,
	         0.01D0, 0.02D0,  0.05D0, 0.1D0,  0.2D0,   0.5D0,   1.0D0,
		 2.0D0, 5.0D0, 10.0D0,20.0D0, 30.0D0, 60.0D0, 120.0d0,
		 300.0D0, 600.0D0, 1.2D3, 1.8D3, 3.6D3, 7.2D3, 1.8D4, 3.6D4 /

begin
	major_interval = wl_check_arrayd (DEGTOSA (latran) / num_try, 
	    decgap, NDECGAP)
	minimum = SATODEG (wl_round_upd (DEGTOSA (latmin), major_interval) -
	    major_interval)
	maximum = SATODEG (wl_round_upd (DEGTOSA (latmax), major_interval))
	major_interval = SATODEG (major_interval)

	# Make sure that the grid marking does not include the pole.
	maximum = min (maximum, NORTH_POLE_LATITUDE - major_interval)
	minimum = max (minimum, SOUTH_POLE_LATITUDE + major_interval)
end


# WL_GENERIC_ROUND -- Round the values (if possible).
#
# History
#   7Feb91 - Created by Jonathan D. Eisenhamer, STScI.

procedure wl_generic_round (minimum, maximum, range, lbegin, lend, interval)

double	minimum, maximum, range  # I: the raw input values
double	lbegin, lend             # O: the begin and end label points
double	interval                 # O: the major label interval

double	amant, diff
int	iexp, num
double	wl_round_upd()

begin
	diff = log10 (abs (range) / 4.D0)
	iexp = int (diff)
	if (diff < 0)
	    iexp = iexp - 1

	amant = diff - double (iexp)
	if (amant < 0.15D0)
	  num = 1
	else if (amant < 0.50D0)
	  num = 2
	else if (amant < 0.85D0)
	  num = 5
	else
	  num = 10

	interval = double (num) * 10.0D0 ** iexp
	lbegin = wl_round_upd (minimum, interval) - interval
	lend = wl_round_upd (maximum, interval)
end


#  WL_ROUND_UPD -- Round X up to nearest whole multiple of Y.

double procedure wl_round_upd (x, y)

double	x		# I: value to be rounded
double	y		# I: multiple of X is to be rounded up in

double	z, r

begin
	if (x < 0.0D0)
	    z = 0.0D0
	else
	    z = y
	r = y * double (int ((x + z) / y))

	return (r)
end



#  WL_CHECK_ARRAYD -- Check proximity of array elements to each other.
#
# Description
#  Returns the element of the array arr(n) which is closest to an exact
#  value EX.

double procedure wl_check_arrayd (ex, arr, n)

double	ex		# I: the exact value
double	arr[ARB]	# I: the array of rounded values
int	n		# I: dimension of array  of rounded values

int	j

begin
	for (j = 1;  j < n && (ex - arr[j]) > 0.0D0;  j = j + 1)
	    ;
	if (j > 1 && j < n)
	    if (abs (ex - arr[j-1]) < abs (ex - arr[j])) 
		j = j - 1

	return (arr[j])
end

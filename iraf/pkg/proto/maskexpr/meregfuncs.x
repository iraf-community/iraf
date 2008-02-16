include <mach.h>
include <ctype.h>
include <math.h>


# ME_POINT -- Compute which pixels are equal to a point.

procedure me_point (ix, iy, stat, npts, x1, y1)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array containing YES or NO
int	npts			#I the number of points
real	x1, y1			#I the coordinates of the point

int	i

begin
	do i = 1, npts {
	    if (ix[i] == nint(x1) && iy[i] == nint(y1))
		stat[i] = YES
	    else
		stat[i] = NO
	}
end


# ME_CIRCLE -- Compute which pixels are within or on a circle.

procedure me_circle (ix, iy, stat, npts, xc, yc, r)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array containing YES or NO
int	npts			#I the number of points
real	xc, yc			#I the center of the circle
real	r			#I the radius of the circle

real	r2, rdist
int	i

begin
	r2 = r * r
	do i = 1, npts {
	    rdist = (ix[i] - xc) ** 2 + (iy[i] - yc) ** 2
	    if (rdist <= r2)
		stat[i] =  YES
	    else
		stat[i] = NO
	}
end


# ME_CANNULUS -- Compute which pixels are within or on a circular annulus
# boundary.

procedure me_cannulus (ix, iy, stat, npts, xc, yc, r1, r2)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array containing YES or NO
int	npts			#I the number of points
real	xc, yc			#I the center of the circle
real	r1, r2			#I the radius of the circular annulus

real	r12, r22, rdist
int	i

begin
	r12 = r1 * r1
	r22 = r2 * r2
	do i = 1, npts {
	    rdist = (ix[i] - xc) ** 2 + (iy[i] - yc) ** 2
	    if (rdist >= r12 && rdist <= r22)
		stat[i] =  YES
	    else
		stat[i] = NO
	}
end


# ME_ELLIPSE -- Compute which pixels lie within or on an ellipse.

procedure me_ellipse (ix, iy, stat, npts, xc, yc, a, ratio, theta)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array (YES/NO)
int	npts			#I the number of points
real	xc, yc			#I the center of the ellipse
real	a			#I the semi-major axis of the ellipse
real	ratio			#I the semi-minor / semi-minor axis
real	theta			#I the position angle of the ellipse

real	asq, bsq, cost, sint, costsq, sintsq, rdist
real	dx, dy, aa, bb, cc, rr
int	i

begin
	asq = a * a
	bsq = (ratio * a) * (ratio * a)
	cost = cos (DEGTORAD(theta))
	sint = sin (DEGTORAD(theta))
	costsq = cost * cost
	sintsq = sint * sint
	aa = bsq * costsq + asq * sintsq
	bb = 2.0 * (bsq - asq) * cost * sint
	cc = asq * costsq + bsq * sintsq
	rr = asq * bsq

	do i = 1, npts {
	    dx = (ix[i] - xc)
	    dy = (iy[i] - yc)
	    rdist = aa * dx * dx + bb * dx * dy + cc * dy * dy
	    if (rdist <= rr)
		stat[i] = YES
	    else
		stat[i] = NO
	}
end


# ME_EANNULUS -- Compute which pixels lie within or on an elliptical annular
# boundary.

procedure me_eannulus (ix, iy, stat, npts, xc, yc, a1, a2, ratio, theta)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array (YES/NO)
int	npts			#I the number of points
real	xc, yc			#I the center of the ellipse
real	a1, a2			#I the semi-major axis of the i/o ellipse
real	ratio			#I the semi-minor / semi-major axis of ellipse
real	theta			#I the position angle of the ellipse

real	a1sq, b1sq, aa1, bb1, cc1, rr1, rdist1
real	a2sq, b2sq, aa2, bb2, cc2, rr2, rdist2
real	dx, dy, cost, sint, costsq, sintsq
int	i

begin
	# First ellipse.
	a1sq = a1 * a1
	b1sq = (ratio * a1) ** 2
	cost = cos (DEGTORAD(theta))
	sint = sin (DEGTORAD(theta))
	costsq = cost * cost
	sintsq = sint * sint
	aa1 = b1sq * costsq + a1sq * sintsq
	bb1 = 2.0 * (b1sq - a1sq) * cost * sint
	cc1 = a1sq * costsq + b1sq * sintsq
	rr1 = a1sq * b1sq

	# Second ellipse.
	a2sq = a2 * a2
	b2sq = (ratio * a2) ** 2
	aa2 = b2sq * costsq + a2sq * sintsq
	bb2 = 2.0 * (b2sq - a2sq) * cost * sint
	cc2 = a2sq * costsq + b2sq * sintsq
	rr2 = a2sq * b2sq

	# Elliptical annulus.
	do i = 1, npts {
	    dx = (ix[i] - xc)
	    dy = (iy[i] - yc)
	    rdist1 = aa1 * dx * dx + bb1 * dx * dy + cc1 * dy * dy
	    rdist2 = aa2 * dx * dx + bb2 * dx * dy + cc2 * dy * dy
	    if (rdist1 >= rr1 && rdist2 <= rr2)
		stat[i] = YES
	    else
		stat[i] = NO
	}
end


# ME_RECTANGLE -- Compute which pixels lie within or on a rectangle.

procedure me_rectangle (ix, iy, stat, npts, xc, yc, a, ratio, theta)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array (YES/NO)
int	npts			#I the number of points
real	xc, yc			#I the center of the rectangle
real	a			#I the semi-major axis width of the rectangle
real	ratio			#I the semi-minor axis / semi-major axis
real	theta			#I the position angle of the rectangle

real	cost, sint, x, y
real	xver[4], yver[4]

begin
	# Compute the corners of the equivalent polygon.
        cost = cos (DEGTORAD(theta))
        sint = sin (DEGTORAD(theta))
        x = a 
        y = ratio * a
        xver[1] = xc + x * cost - y * sint
        yver[1] = yc + x * sint + y * cost
        x = -x
        y = y
        xver[2] = xc + x * cost - y * sint
        yver[2] = yc + x * sint + y * cost
        x = x
        y = -y
        xver[3] = xc + x * cost - y * sint
        yver[3] = yc + x * sint + y * cost
        x = -x
        y = y
        xver[4] = xc + x * cost - y * sint
        yver[4] = yc + x * sint + y * cost

	# Call the polygon routine.
	call me_polygon (ix, iy, stat, npts, xver, yver, 4)
end


# ME_RANNULUS -- Compute which pixels lie within or on a rectangular annulus.

procedure me_rannulus (ix, iy, stat, npts, xc, yc, r1, r2, ratio, theta)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array (YES/NO)
int	npts			#I the number of points
real	xc, yc			#I the center of the rectangle
real	r1, r2			#I the semi-major axis width of the rectangle
real	ratio			#I the semi-minor / semi-major axis ratio
real	theta			#I the position angle of the rectangle

real	cost, sint, x, y, xver1[4], yver1[4], xver2[4], yver2[4]

begin
	# Compute the corners of the equivalent polygon inner and outer
	# polygons.
        cost = cos (DEGTORAD(theta))
        sint = sin (DEGTORAD(theta))

	# The corners of the inner polygon.
        x = r1 
        y = ratio * r1
        xver1[1] = xc + x * cost - y * sint
        yver1[1] = yc + x * sint + y * cost
        x = -x
        y = y
        xver1[2] = xc + x * cost - y * sint
        yver1[2] = yc + x * sint + y * cost
        x = x
        y = -y
        xver1[3] = xc + x * cost - y * sint
        yver1[3] = yc + x * sint + y * cost
        x = -x
        y = y
        xver1[4] = xc + x * cost - y * sint
        yver1[4] = yc + x * sint + y * cost

	# The corners of the outer polygon.
        x = r2 
        y = ratio * r2 
        xver2[1] = xc + x * cost - y * sint
        yver2[1] = yc + x * sint + y * cost
        x = -x
        y = y
        xver2[2] = xc + x * cost - y * sint
        yver2[2] = yc + x * sint + y * cost
        x = x
        y = -y
        xver2[3] = xc + x * cost - y * sint
        yver2[3] = yc + x * sint + y * cost
        x = -x
        y = y
        xver2[4] = xc + x * cost - y * sint
        yver2[4] = yc + x * sint + y * cost

	# Write a routine to determine which pixels are inside the  polygon
	# defined by 2 sets of vertices.
	call me_apolygon (ix, iy, stat, npts, xver1, yver1, xver2, yver2, 4)
end


# ME_BOX -- Compute which pixels lie within or on a box.

procedure me_box (ix, iy, stat, npts, x1, y1, x2, y2)

int	ix[ARB]			#I the integer x coordinates
int	iy[ARB]			#I the integer y coordinates
int	stat[ARB]		#O the integer status array (YES/NO)
int	npts			#I the number of points
real	x1, y1			#I first box corner
real	x2, y2			#I first box corner

real	xmin, xmax, ymin, ymax
int	i

begin
	xmin = min (x1, x2)
	xmax = max (x1, x2)
	ymin = min (y1, y2)
	ymax = max (y1, y2)

	do i = 1, npts {
	    if (ix[i] >= xmin && ix[i] <= xmax &&
	        iy[i] >= ymin && iy[i] <= ymax)
		stat[i] = YES
	    else
		stat[i] = NO
	}
end


# ME_POLYGON -- Determine which points lie in or on a specified polygon. 

procedure me_polygon (ix, iy, stat, npts, xver, yver, nver)

int	ix[ARB]			#I the x image pixel coordinates
int	iy[ARB]			#I the y image pixel coordinates
int	stat[ARB]		#O the output status array
int	npts			#I the number of image pixel coordinates
real	xver[ARB]		#I the x polygon vertices coordinates
real	yver[ARB]		#I the y polygon vertices coordinates
int	nver			#I the number of polygon coordinates

real	lx, ld
pointer	sp, txver, tyver, work1, work2, xintr
int	i, j, ixmin, ixmax, nintr
int	me_pyclip()

begin
	call smark (sp)
	call salloc (txver, nver + 1, TY_REAL)
	call salloc (tyver, nver + 1, TY_REAL)
	call salloc (work1, nver + 1, TY_REAL)
	call salloc (work2, nver + 1, TY_REAL)
	call salloc (xintr, nver + 1, TY_REAL)

	# Close the polygon.
	call amovr (xver, Memr[txver], nver)
	call amovr (yver, Memr[tyver], nver)
	Memr[txver+nver] = xver[1] 
	Memr[tyver+nver] = yver[1] 

	# Loop over the points.
	call alimi (ix, npts, ixmin, ixmax)
	lx = ixmax - ixmin + 1
	do i = 1, npts {

	    # Compute the intersection points of the line segment which
	    # spans an image line with the polygon. Sort the line segments.
	    ld = iy[i]
	    if (i == 1)  {
		nintr = me_pyclip (Memr[txver], Memr[tyver], Memr[work1],
		    Memr[work2], Memr[xintr], nver + 1, lx, ld)
		call asrtr (Memr[xintr], Memr[xintr], nintr)
	    } else if (iy[i] != iy[i-1]) {
		nintr = me_pyclip (Memr[txver], Memr[tyver], Memr[work1],
		    Memr[work2], Memr[xintr], nver + 1, lx, ld)
		call asrtr (Memr[xintr], Memr[xintr], nintr)
	    }

	    # Are the intersection points in range ?
	    if (nintr <= 0)
		stat[i] = NO
	    else {
		stat[i] = NO
		do j = 1, nintr, 2 {
		    if (ix[i] >= Memr[xintr+j-1] && ix[i] <= Memr[xintr+j]) 
			stat[i] = YES
		}
	    }
	    
	}

	call sfree (sp)
end


# ME_APOLYGON -- Determine which points lie in or on a specified polygonal
# annulus. 

procedure me_apolygon (ix, iy, stat, npts, ixver, iyver, oxver, oyver, nver)

int	ix[ARB]			#I the x image pixel coordinates
int	iy[ARB]			#I the y image pixel coordinates
int	stat[ARB]		#O the output status array
int	npts			#I the number of image pixel coordinates
real	ixver[ARB]		#I the x polygon vertices coordinates
real	iyver[ARB]		#I the y polygon vertices coordinates
real	oxver[ARB]		#I the x polygon vertices coordinates
real	oyver[ARB]		#I the y polygon vertices coordinates
int	nver			#I the number of polygon coordinates

real	lx, ld
pointer	sp, tixver, tiyver, toxver, toyver, work1, work2, ixintr, oxintr
int	i, j, jj, ixmin, ixmax, inintr, onintr, ibegin, iend
int	me_pyclip()

begin
	call smark (sp)
	call salloc (tixver, nver + 1, TY_REAL)
	call salloc (tiyver, nver + 1, TY_REAL)
	call salloc (toxver, nver + 1, TY_REAL)
	call salloc (toyver, nver + 1, TY_REAL)
	call salloc (work1, nver + 1, TY_REAL)
	call salloc (work2, nver + 1, TY_REAL)
	call salloc (ixintr, nver + 1, TY_REAL)
	call salloc (oxintr, nver + 1, TY_REAL)

	# Close the polygons.
	call amovr (ixver, Memr[tixver], nver)
	call amovr (iyver, Memr[tiyver], nver)
	Memr[tixver+nver] = ixver[1] 
	Memr[tiyver+nver] = iyver[1] 
	call amovr (oxver, Memr[toxver], nver)
	call amovr (oyver, Memr[toyver], nver)
	Memr[toxver+nver] = oxver[1] 
	Memr[toyver+nver] = oyver[1] 

	# Loop over the points.
	call alimi (ix, npts, ixmin, ixmax)
	lx = ixmax - ixmin + 1
	do i = 1, npts {

	    stat[i] = NO

	    # Compute the intersection points of the line segment with the
	    # outer polygon.
	    ld = iy[i]
	    if (i == 1)  {
		onintr = me_pyclip (Memr[toxver], Memr[toyver], Memr[work1],
		    Memr[work2], Memr[oxintr], nver + 1, lx, ld)
		call asrtr (Memr[oxintr], Memr[oxintr], onintr)
	    } else if (iy[i] != iy[i-1]) {
		onintr = me_pyclip (Memr[toxver], Memr[toyver], Memr[work1],
		    Memr[work2], Memr[oxintr], nver + 1, lx, ld)
		call asrtr (Memr[oxintr], Memr[oxintr], onintr)
	    }
	    if (onintr <= 0)
		next

	    # Compute the intersection points of the line segment with the
	    # inner polygon.
	    if (i == 1)  {
		inintr = me_pyclip (Memr[tixver], Memr[tiyver], Memr[work1],
		    Memr[work2], Memr[ixintr], nver + 1, lx, ld)
		call asrtr (Memr[ixintr], Memr[ixintr], inintr)
	    } else if (iy[i] != iy[i-1]) {
		inintr = me_pyclip (Memr[tixver], Memr[tiyver], Memr[work1],
		    Memr[work2], Memr[ixintr], nver + 1, lx, ld)
		call asrtr (Memr[ixintr], Memr[ixintr], inintr)
	    }

	    # Are the intersection points in range ?
	    if (inintr <= 0) {
		do j = 1, onintr, 2 {
		    if (ix[i] >= Memr[oxintr+j-1] && ix[i] <= Memr[oxintr+j]) {
			stat[i] = YES
			break
		    }
		}
	    } else {
		do j = 1, onintr, 2 {
		    do jj = 1, inintr, 2 {
			if ((Memr[ixintr+jj-1] > Memr[oxintr+j-1]) &&
			    (Memr[ixintr+jj-1] < Memr[oxintr+j])) {
			    ibegin = jj
			    break
			}
		    }
		    do jj = inintr, 1, -2 {
			if ((Memr[ixintr+jj-1] > Memr[oxintr+j-1]) &&
			    (Memr[ixintr+jj-1] < Memr[oxintr+j])) {
			    iend = jj
			    break
			}
		    }
		    if ((ix[i] >= Memr[oxintr+j-1]) &&
		        (ix[i] <= Memr[ixintr+ibegin-1])) {
			stat[i] = YES
		    } else if ((ix[i] >= Memr[ixintr+iend-1]) &&
		        (ix[i] <= Memr[oxintr+j])) {
			stat[i] = YES
		    } else {
			do jj = ibegin + 1, iend - 1, 2 {
			    if ((ix[i] >= Memr[ixintr+jj-1]) &&
			        (ix[i] <= Memr[ixintr+jj])) {
				stat[i] = YES
				break
			    }
			}
		    }

		}
	    }
	    
	}

	call sfree (sp)
end


define	MAX_NRANGES	100

# ME_COLS -- Determine which pixels are in the specified column ranges.

procedure me_cols (ix, stat, npts, rangstr)

int	ix[ARB]			#I the x image pixel coordinates
int	stat[ARB]		#O the output status array
int	npts			#I the number of image pixel coordinates
char	rangstr[ARB]		#I the input range specification string

pointer	sp, ranges
int	index, nvals
int	me_decode_ranges(), me_next_number()

begin
	# Allocate space for storing the ranges.
	call smark (sp)
	call salloc (ranges, 3 * MAX_NRANGES + 1, TY_INT)

	# Decode the ranges string. If there was an error set up the ranges
	# so as to include everything.
	if (me_decode_ranges (rangstr, Memi[ranges], MAX_NRANGES,
	    nvals) == ERR) {
	    if (me_decode_ranges ("-", Memi[ranges], MAX_NRANGES, nvals) != ERR)
		;
	}

	# Set the status array.
	call amovki (NO, stat, npts)
	index = 0
	while (me_next_number (Memi[ranges], index) != EOF)
	    stat[index] = YES

	call sfree (sp)
end


# ME_LINES -- Determine which pixels are in the specified column ranges.

procedure me_lines (ix, stat, npts, rangstr)

int	ix[ARB]			#I the x image pixel coordinates
int	stat[ARB]		#O the output status array
int	npts			#I the number of image pixel coordinates
char	rangstr[ARB]		#I the input range specification string

pointer	sp, ranges
int	i, lastix, nvals
int	me_decode_ranges()
bool	me_is_in_range()

begin
	# Allocate space for storing the ranges.
	call smark (sp)
	call salloc (ranges, 3 * MAX_NRANGES + 1, TY_INT)

	# Decode the ranges string. If there was an error set up the ranges
	# so as to include everything.
	if (me_decode_ranges (rangstr, Memi[ranges], MAX_NRANGES,
	    nvals) == ERR) {
	    if (me_decode_ranges ("-", Memi[ranges], MAX_NRANGES, nvals) != ERR)
		;
	}

	# Set the line numbers.
	call amovki (NO, stat, npts)
	lastix = 0
	do i = 1, npts {
	    if (ix[i] == lastix) {
		stat[i] = YES
	    } else if (me_is_in_range (Memi[ranges], ix[i])) {
		lastix = ix[i]
		stat[i] = YES
	    }
	}

	call sfree (sp)
end


# ME_VECTOR -- Determine which pixels are on the specified line.

procedure me_vector (ix, iy, stat, npts, x1, y1, x2, y2, width)

int	ix[ARB]			#I the x image pixel coordinates
int	iy[ARB]			#I the y image pixel coordinates
int	stat[ARB]		#O the output status array
int	npts			#I the number of image pixel coordinates
real	x1, y1			#I coordinates of the first point
real	x2, y2			#I coordinates of the first point
real	width			#I the vector width

real	x, y, xc, yc, theta, cost, sint
real	xver[4], yver[4]

begin
	# Compute the corners of the equivalent polygon.
	xc = (x2 + x1) / 2.0
	yc = (y2 + y1) / 2.0
        x = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2) / 2.0 
        y = width / 2.0
	theta = atan2 (y2 - y1, x2 - x1)
        cost = cos (theta)
        sint = sin (theta)
        xver[1] = xc + x * cost - y * sint
        yver[1] = yc + x * sint + y * cost
        x = -x
        y = y
        xver[2] = xc + x * cost - y * sint
        yver[2] = yc + x * sint + y * cost
        x = x
        y = -y
        xver[3] = xc + x * cost - y * sint
        yver[3] = yc + x * sint + y * cost
        x = -x
        y = y
        xver[4] = xc + x * cost - y * sint
        yver[4] = yc + x * sint + y * cost

	# Call the polygon routine.
	call me_polygon (ix, iy, stat, npts, xver, yver, 4)
end


define	SMALL_NUMBER	1.0e-24

# ME_PIE -- Determine which pixels are inside a pie shaped wedge that
# intersects the image boundaries. 

procedure me_pie (ix, iy, stat, npts, xc, yc, angle1, angle2, width, height)

int	ix[ARB]			#I the x pixel coordinates
int	iy[ARB]			#I the y pixel coordinates
int	stat[ARB]		#O the output status array 
int	npts			#I the number of data points
real	xc, yc			#I the center of the wedge
real	angle1, angle2		#I the wedge angles
int	width, height		#I the image mask width and height

real	sweep, x2, y2, vx[7], vy[7]
int	count, intrcpt1, intrcpt2
int	me_pie_intercept(), me_corner_vertex()

begin
	# Set the first vertex
	vx[1] = xc
	vy[1] = yc
	sweep = angle2 - angle1

	# If the sweep is too small to be noticed don't bother.
	if (abs (sweep) < SMALL_NUMBER) {
	    call amovki (NO, stat, npts) 
	    return
	}
	if (sweep < 0.0)
	    sweep = sweep + 360.0

	# Get the second vertext by computing the intersection of the
	# first ray with the image boundaries.
	intrcpt1 = me_pie_intercept (width, height, xc, yc, angle1,
	    vx[2], vy[2])

	# Compute the second intercept.
	intrcpt2 = me_pie_intercept (width, height, xc, yc, angle2, x2, y2)

	# If angles intercept same side and slice is between them, no corners
	# else, mark corners until reaching side with second angle intercept.
	count = 3
	if ((intrcpt1 != intrcpt2) || (sweep > 180.0)) {
	    repeat {
		intrcpt1 = me_corner_vertex (intrcpt1, width, height, vx[count],
		    vy[count])
		count = count + 1
	    } until (intrcpt1 == intrcpt2)
	}

	# Set last vertex.
	vx[count] = x2
	vy[count] = y2

	# Fill in the polygon
	call me_polygon (ix, iy, stat, npts, vx, vy, count)
end


# ME_PIE_INTERCEPT --  Determine which side is intercepted by a vertex (given
# center and angle) and set edge intercept point and return index of side.

int procedure me_pie_intercept (width, height, xcen, ycen, angle, xcept, ycept)

int	width, height		#I the dimensions of the image field
real	xcen, ycen		#I the base pivot point of the ray
real	angle			#I the angle of ray
real	xcept, ycept		#I coordinates of intercept with edge of image

real	angl, slope

begin
	# Put angles in normal range.
	angl = angle
	while (angl < 0.0)
	    angl = angl + 360.0
	while (angl >= 360.0)
	    angl = angl - 360.0

	# Check for a horizontal angle.
	if (abs (angl) < SMALL_NUMBER) {
	    #xcept = 0
	    xcept = width + 1
	    ycept = ycen
	    #return (2)
	    return (4)
	}
	if (abs (angl - 180.0) < SMALL_NUMBER) {
	    #xcept = width + 1
	    xcept = 0
	    ycept = ycen
	    #return (4)
	    return (2)
	}

#	# Convert to a Cartesian angle
#	angl = angl + 90.0
#	if (angl >= 360.0)
#	    angl = angl - 360.0

	# Check for vertical angle.
	if (angl < 180.0) {
	    ycept = height + 1
	    # rule out vertical line
	    if (abs(angl - 90.0) < SMALL_NUMBER) {
		x_cept = xcen
		return (1)
	    }
	} else {
	    ycept = 0.0
	    # rule out vertical line
	    if (abs(angl - 270.0) < SMALL_NUMBER) {
		xcept = xcen
		return (3)
	    }
	}

	# Convert to radians.
	angl = (angl / 180.0) * PI

	# Calculate slope.
	slope = tan (angl)

	# Calculate intercept with designated y edge.
	xcept = xcen + ((ycept - ycen) / slope)
	if (xcept < 0) {
	    ycept = (ycen - (xcen * slope))
	    xcept = 0.0
	    return (2)
	} else if (xcept > (width + 1)) {
	    ycept = (ycen + ((width + 1 - xcen) * slope))
	    xcept = width + 1
	    return (4)
	} else {
	    if (ycept < height)
		return (3)
	    else
		return (1)
	}
end


# ME_CORNER_VERTEX -- Set points just beyond corner to mark the corner in a
# polygon. Note: 1=top, 2=left, 3=bottom, 4=right, corner is between current
# and next  advance index to next side and also return its value.

int procedure me_corner_vertex (index, width, height, x, y)

int	index			#I  code of side before corner
int	width, height		#I  dimensions of image field
real	x, y			#O  coords of corner

begin
	# Set the corner coordinates.
	switch (index) {
	case 1:
	    x = 0.0
	    y = height + 1
	case 2:
	    x = 0.0
	    y = 0.0
	case 3:
	    x = width + 1
	    y = 0.0
	case 4:
	    x = width + 1
	    y = height + 1
	default:
	    ; #call error (1, "index error in mark_corner")
	}

	# Set the corner index.
	index = index + 1
	if (index > 4)
	    index = 1

	return (index)
end


# ME_PYEXPAND -- Expand a polygon given a list of vertices and an expansion
# factor in pixels.

procedure me_pyexpand (xin, yin, xout, yout, nver, width)

real    xin[ARB]                #I the x coordinates of the input vertices
real    yin[ARB]                #I the y coordinates of the input vertices
real    xout[ARB]               #O the x coordinates of the output vertices
real    yout[ARB]               #O the y coordinates of the output vertices
int     nver                    #I the number of vertices
real    width                   #I the width of the expansion region

real    xcen, ycen, m1, b1, m2, b2, xp1, yp1, xp2, yp2
int     i
real    asumr()

begin
        # Find the center of gravity of the polygon.
        xcen = asumr (xin, nver) / nver
        ycen = asumr (yin, nver) / nver

        do i = 1, nver {

            # Compute the equations of the line segments parallel to the
            # line seqments composing a single vertex.
            if (i == 1) {
                call me_psegment (xcen, ycen, xin[nver], yin[nver], xin[1],
                    yin[1], width, m1, b1, xp1, yp1)
                call me_psegment (xcen, ycen, xin[1], yin[1], xin[2], yin[2],
                    width, m2, b2, xp2, yp2)
            } else if (i == nver) {
                call me_psegment (xcen, ycen, xin[nver-1], yin[nver-1],
                    xin[nver], yin[nver], width, m1, b1, xp1, yp1)
                call me_psegment (xcen, ycen, xin[nver], yin[nver], xin[1],
                    yin[1], width, m2, b2, xp2, yp2)
            } else {
                call me_psegment (xcen, ycen, xin[i-1], yin[i-1], xin[i],
                    yin[i], width, m1, b1, xp1, yp1)
                call me_psegment (xcen, ycen, xin[i], yin[i], xin[i+1],
                    yin[i+1], width, m2, b2, xp2, yp2)
            }

            # The new vertex is the intersection of the two new line
            # segments.
            if (m1 == m2) {
                xout[i] = xp2
                yout[i] = yp2
            } else if (IS_INDEFR(m1)) {
                xout[i] = xp1
                yout[i] = m2 * xp1 + b2
            } else if (IS_INDEFR(m2)) {
                xout[i] = xp2
                yout[i] = m1 * xp2 + b1
            } else {
                xout[i] = (b2 - b1) / (m1 - m2)
                yout[i] = (m2 * b1 - m1 * b2) / (m2 - m1)
            }
        }
end


# ME_PSEGMENT -- Construct a line segment parallel to an existing line segment
# but a specified distance from it in a direction away from a fixed reference
# point.

procedure me_psegment (xcen, ycen, xb, yb, xe, ye, width, m, b, xp, yp)

real    xcen, ycen              #I the position of the reference point
real    xb, yb                  #I the starting coordinates of the line segment
real    xe, ye                  #I the ending coordinates of the line segment
real    width                   #I the distance of new line segment from old
real    m                       #O the slope of the new line segment
real    b                       #O the intercept of the new line segment
real    xp, yp                  #O the coordinates of a points on new line

real    x1, y1, x2, y2, d1, d2

begin
        # Compute the slope of the line segment.
        m = (xe - xb)
        if (m == 0.0)
            m = INDEFR
        else
            m = (ye - yb) / m

        # Construct the perpendicular to the line segement and locate two
        # points which are equidistant from the line seqment
        if (IS_INDEFR(m)) {
            x1 = xb - width
            y1 = yb
            x2 = xb + width
            y2 = yb
        } else if (m == 0.0) {
            x1 = xb
            y1 = yb - width
            x2 = xb
            y2 = yb + width
        } else {
            x1 = xb - sqrt ((m * width) ** 2 / (m ** 2 + 1))
            y1 = yb - (x1 - xb) / m
            x2 = xb + sqrt ((m * width) ** 2 / (m ** 2 + 1))
            y2 = yb - (x2 - xb) / m
        }

        # Choose the point farthest away from the reference point.
        d1 = (x1 - xcen) ** 2 + (y1 - ycen) ** 2
        d2 = (x2 - xcen) ** 2 + (y2 - ycen) ** 2
        if (d1 <= d2) {
            xp = x2
            yp = y2
        } else {
            xp = x1
            yp = y1
        }

        # Compute the intercept.
        if (IS_INDEFR(m))
            b = INDEFR
        else
            b = yp - m * xp
end


# ME_PYCLIP -- Compute the intersection of an image line with a polygon defined
# by a list of vertices.  The output is a list of ranges stored in the array
# xranges. Two additional work arrays xintr and slope are required for
# the computation.

int procedure me_pyclip (xver, yver, xintr, slope, xranges, nver, lx, ld)

real    xver[ARB]               #I the x vertex coords
real    yver[ARB]               #I the y vertex coords
real    xintr[ARB]              #O the array of x intersection points
real    slope[ARB]              #O the array of y slopes at intersection points
real    xranges[ARB]            #O the x line segments
int     nver                    #I the number of vertices
real    lx, ld                  #I the equation of the image line

real    u1, u2, u1u2, dx, dy, dd, xa, wa
int     i, j, nintr, nplus, nzero, nneg, imin, imax, nadd
bool    collinear

begin
	# Initialize.
        collinear = false
        nplus = 0
        nzero = 0
        nneg = 0
        nintr = 0

        # Compute the intersection points of the image line and the polygon.
        u1 = lx * (- yver[1] + ld)
        do i = 2, nver {

            u2 = lx * (- yver[i] + ld)
            u1u2 = u1 * u2

            # Does the polygon side intersect the image line ?
            if (u1u2 <= 0.0) {


                # Compute the x intersection coordinate if the point of
                # intersection is not a vertex.

                if ((u1 != 0.0) && (u2 != 0.0)) {

                    dy = yver[i-1] - yver[i]
                    dx = xver[i-1] - xver[i]
                    dd = xver[i-1] * yver[i] - yver[i-1] * xver[i]
                    xa = lx * (dx * ld - dd)
                    wa = dy * lx
                    nintr = nintr + 1
                    xranges[nintr] = xa / wa
                    slope[nintr] = -dy
                    if (slope[nintr] < 0.0)
                        nneg = nneg + 1
                    else if (slope[nintr] > 0.0)
                        nplus = nplus + 1
                    else
                        nzero = nzero + 1
                    collinear = false

                # For each collinear line segment add two intersection
                # points. Remove interior collinear intersection points.

                } else if (u1 == 0.0 && u2 == 0.0) {

                    if (! collinear) {
                        nintr = nintr + 1
                        xranges[nintr] = xver[i-1]
                        if (i == 2)
                            slope[nintr] = yver[1] - yver[nver-1]
                        else
                            slope[nintr] = yver[i-1] - yver[i-2]
                        if (slope[nintr] < 0.0)
                            nneg = nneg + 1
                        else if (slope[nintr] > 0.0)
                            nplus = nplus + 1
                        else
                            nzero = nzero + 1
                        nintr = nintr + 1
                        xranges[nintr] = xver[i]
                        slope[nintr] = 0.0
                        nzero = nzero + 1
                    } else {
                        xranges[nintr] = xver[i]
                        slope[nintr] = 0.0
                        nzero = nzero + 1
                    }
                    collinear = true

                # If the intersection point is a vertex add it to the
                # list if it is not collinear with the next point. Add
                # another point to the list if the vertex is at the
                # apex of an acute angle.

                } else if (u1 != 0.0) {

                    if (i == nver) {
                        dx = (xver[2] - xver[nver])
                        dy = (yver[2] - yver[nver])
                        dd = dy * (yver[nver-1] - yver[nver])
                    } else {
                        dx = (xver[i+1] - xver[i])
                        dy = (yver[i+1] - yver[i])
                        dd = dy * (yver[i-1] - yver[i])
                    }

                    # Test whether the point is collinear with the point
                    # ahead. If it is not include the intersection point.

                    if (dy != 0.0) {
                        nintr = nintr + 1
                        xranges[nintr] = xver[i]
                        slope[nintr] = yver[i] - yver[i-1]
                        if (slope[nintr] < 0.0)
                            nneg = nneg + 1
                        else if (slope[nintr] > 0.0)
                            nplus = nplus + 1
                        else
                            nzero = nzero + 1
                    }

                    # If the intersection point is an isolated vertex add
                    # another point to the list.

                    if (dd > 0.0) {
                        nintr = nintr + 1
                        xranges[nintr] = xver[i]
                        slope[nintr] = dy
                        if (slope[nintr] < 0.0)
                            nneg = nneg + 1
                        else if (slope[nintr] > 0.0)
                            nplus = nplus + 1
                        else
                            nzero = nzero + 1
                    }

                    collinear = false

                } else
                    collinear = false
            } else
                collinear = false

            u1 = u2
        }

        # Join up any split collinear line segments.
        if (collinear && (slope[1] == 0.0)) {
            xranges[1] = xranges[nintr-1]
            slope[1] = slope[nintr-1]
            nintr = nintr - 2
            nzero = nzero - 2
        }

        # Return the number of intersection points if there are no interior
        # collinear line segments.
        if (nzero == 0 || nplus == 0 || nneg == 0)
            return (nintr)

        # Find the minimum and maximum intersection points.
        call me_alimr (xranges, nintr, u1, u2, imin, imax)

        # Check for vertices at the ends of the ranges.

        u1 = xranges[min(imin,imax)] - xranges[1]
        u2 = xranges[nintr] - xranges[max(imin,imax)]

        # Vertices were traversed in order of increasing x.
        if ((u1 >= 0.0 && u2 > 0.0) || (u1 > 0.0 && u2 >= 0.0) ||
            (u1 == u2 && imax > imin)) {
            do i = imax + 1, nintr {
                if (xranges[i] != xranges[i-1])
                    break
                imax = i
            }
            do i = imin - 1, 1, -1 {
                if (xranges[i] != xranges[i+1])
                    break
                imin = i
            }
        }

        # Vertices were traversed in order of decreasing x.
        if ((u1 <= 0.0 && u2 < 0.0) || (u1 < 0.0 && u2 <= 0.0) ||
            (u1 == u2 && imax < imin)) {
            do i = imin + 1, nintr {
                if (xranges[i] != xranges[i-1])
                    break
                imin = i
            }
            do i = imax - 1, 1, -1 {
                if (xranges[i] != xranges[i+1])
                    break
                imax = i
            }
        }

        # Reorder the x ranges and slopes if necessary.
        if ((imax < imin) && ! (imin == nintr && imax == 1)) {
            call amovr (xranges, xintr, nintr)
            do i = 1, imax
                xranges[nintr-imax+i] = xintr[i]
            do i = imin, nintr
                xranges[i-imax] = xintr[i]
            call amovr (slope, xintr, nintr)
            do i = 1, imax
                slope[nintr-imax+i] = xintr[i]
            do i = imin, nintr
                slope[i-imax] = xintr[i]
        } else if ((imin < imax) && ! (imin == 1 && imax == nintr)) {
            call amovr (xranges, xintr, nintr)
            do i = 1, imin
                xranges[nintr-imin+i] = xintr[i]
            do i = imax, nintr
                xranges[i-imin] = xintr[i]
            call amovr (slope, xintr, nintr)
            do i = 1, imin
                slope[nintr-imin+i] = xintr[i]
            do i = imax, nintr
                slope[i-imin] = xintr[i]
        }

        # Add any extra intersection points that are required to deal with
        # the collinear line segments.

        nadd = 0
        for (i = 1; i <= nintr-2; ) {
            if (slope[i] * slope[i+2] > 0.0) {
                i = i + 2
            } else {
                nadd = nadd + 1
                xranges[nintr+nadd] = xranges[i+1]
                for (j = i + 3; j <= nintr; j = j + 1) {
                    if (slope[i] * slope[j] > 0)
                        break
                    nadd = nadd + 1
                    xranges[nintr+nadd] = xranges[j-1]
                }
                i = j
            }
        }

        return (nintr + nadd)
end


# ME_ALIMR -- Compute the maximum and minimum data values and indices of a
# 1D array.

procedure me_alimr (data, npts, mindat, maxdat, imin, imax)

real    data[npts]      #I the input data array
int     npts            #I the number of points
real    mindat, maxdat  #O the minimum and maximum data values
int     imin, imax      #O the indices of the minimum and maximum data values

int     i

begin
        imin = 1
        imax = 1
        mindat = data[1]
        maxdat = data[1]

        do i = 2, npts {
            if (data[i] > maxdat) {
                imax = i
                maxdat = data[i]
            }
            if (data[i] < mindat) {
                imin = i
                mindat = data[i]
            }
        }
end


define	FIRST	1		# Default starting range
define	LAST	MAX_INT		# Default ending range
define	STEP	1		# Default step
define	EOLIST	0		# End of list

# ME_DECODE_RANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Return as output a list
# of ranges defining a list of numbers, and the count of list numbers.
# Range limits must be positive nonnegative integers.  ERR is returned as
# the function value if a conversion error occurs.  The list of ranges is
# delimited by EOLIST.

int procedure me_decode_ranges (range_string, ranges, max_ranges, nvalues)

char	range_string[ARB]	#I range string to be decoded
int	ranges[3, max_ranges]	#O output range array
int	max_ranges		#I maximum number of ranges
int	nvalues			#O the number of values in the ranges

int	ip, nrange, first, last, step, ctoi()

begin
	ip = 1
	nvalues = 0

	do nrange = 1, max_ranges - 1 {
	    # Defaults to all nonnegative integers
	    first = FIRST
	    last = LAST
	    step = STEP

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get first limit.
	    # Must be a number, '-', 'x', or EOS.  If not return ERR.
	    if (range_string[ip] == EOS) {			# end of list
		if (nrange == 1) {
		    # Null string defaults
		    ranges[1, 1] = first
		    ranges[2, 1] = last
		    ranges[3, 1] = step
		    ranges[1, 2] = EOLIST
	    	    nvalues = MAX_INT
		    return (OK)
		} else {
		    ranges[1, nrange] = EOLIST
		    return (OK)
		}
	    } else if (range_string[ip] == '-')
		;
	    else if (range_string[ip] == 'x')
		;
	    else if (IS_DIGIT(range_string[ip])) {		# ,n..
		if (ctoi (range_string, ip, first) == 0)
		    return (ERR)
	    } else
		return (ERR)

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get last limit
	    # Must be '-', or 'x' otherwise last = first.
	    if (range_string[ip] == 'x')
		;
	    else if (range_string[ip] == '-') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, last) == 0)
		        return (ERR)
		} else if (range_string[ip] == 'x')
		    ;
		else
		    return (ERR)
	    } else
		last = first

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get step.
	    # Must be 'x' or assume default step.
	    if (range_string[ip] == 'x') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, step) == 0)
		        ;
		    if (step == 0)
			return (ERR)
		} else if (range_string[ip] == '-')
		    ;
		else
		    return (ERR)
	    }

	    # Output the range triple.
	    ranges[1, nrange] = first
	    ranges[2, nrange] = last
	    ranges[3, nrange] = step
	    nvalues = nvalues + abs (last-first) / step + 1
	}

	return (ERR)					# ran out of space
end


# ME_NEXT_NUMBER -- Given a list of ranges and the current file number,
# find and return the next file number.  Selection is done in such a way
# that list numbers are always returned in monotonically increasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure me_next_number (ranges, number)

int	ranges[ARB]		#I the range array
int	number			#U both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number+1 is anywhere in the list, that is the next number,
	# otherwise the next number is the smallest number in the list which
	# is greater than number+1.

	number = number + 1
	next_number = MAX_INT

	for (ip=1;  ranges[ip] != EOLIST;  ip=ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (step == 0)
		call error (1, "Step size of zero in range list")
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder + step <= last)
		    next_number = number - remainder + step
	    } else if (first > number)
		next_number = min (next_number, first)
	}

	if (next_number == MAX_INT)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# ME_PREVIOUS_NUMBER -- Given a list of ranges and the current file number,
# find and return the previous file number.  Selection is done in such a way
# that list numbers are always returned in monotonically decreasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure me_previous_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number-1 is anywhere in the list, that is the previous number,
	# otherwise the previous number is the largest number in the list which
	# is less than number-1.

	number = number - 1
	next_number = 0

	for (ip=1;  ranges[ip] != EOLIST;  ip=ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (step == 0)
		call error (1, "Step size of zero in range list")
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder >= first)
		    next_number = number - remainder
	    } else if (last < number) {
		remainder = mod (last - first, step)
		if (remainder == 0)
		    next_number = max (next_number, last)
		else if (last - remainder >= first)
		    next_number = max (next_number, last - remainder)
	    }
	}

	if (next_number == 0)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# ME_IS_IN_RANGE -- Test number to see if it is in range. If the number is
# INDEFI then it is mapped to the maximum integer.

bool procedure me_is_in_range (ranges, number)

int	ranges[ARB]		# range array
int	number			# number to be tested against ranges

int	ip, first, last, step, num

begin
	if (IS_INDEFI (number))
	    num = MAX_INT
	else
	    num = number

	for (ip = 1;  ranges[ip] != EOLIST;  ip = ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (num >= first && num <= last)
		if (mod (num - first, step) == 0)
		    return (true)
	}

	return (false)
end

include <math.h>
include <plset.h>
include <plio.h>


# PE_POINT -- Rasterop between a point region as source and an existing
# mas as destination.

procedure pe_point (pl, x, y, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of circle
int	rop			#I rasterop

begin
	call pl_point (pl, nint(x), nint(y), rop)
end


define	LEN_CIRCLEDES	5
define	C_PL		Memi[$1]	# reference mask
define	C_XCEN		Memr[$1+1]	# X center of circle
define	C_YCEN		Memr[$1+2]	# Y center of circle
define	C_RADIUS	Memr[$1+3]	# radius of circle
define	C_PV		Memi[$1+4]	# pixel value

# PE_CIRCLE -- Rasterop between a circular region as source and an existing
# mask as destination.  It is not necessary for the center of the circle to
# be inside the mask; if it is outside, the boundary of the circle will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified. These routines are
# a modification of the ones in plio$plcircle. The main difference is
# that the x, y, radius parameters are initially set to real numbers not
# integers.

procedure pe_circle (pl, x, y, radius, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of circle
real	radius			#I radius of circle
int	rop			#I rasterop

real	y1r, y2r, x1r, x2r
int	y1, y2
pointer	sp, ufd
extern	pe_ucircle()

begin
	# Not sure why we need to call this routine here.
	#call plvalid (pl)

	# Test the line and column limits.
	y1r = y - radius
	y2r = y + radius
	x1r = x - radius
	x2r = x + radius
	if ((y2r < 0.5) || (y1r > PL_AXLEN(pl,2) + 0.5))
	    return
	if ((x2r < 0.5) || (x1r > PL_AXLEN(pl,1) + 0.5))
	    return

	call smark (sp)
	call salloc (ufd, LEN_CIRCLEDES, TY_STRUCT)

	y1 = max ( 1, min (PL_AXLEN(pl,2), int(y1r)))
	y2 = max (y1, min (PL_AXLEN(pl,2), int(y2r)))

	C_PL(ufd) = pl
	C_XCEN(ufd) = x
	C_YCEN(ufd) = y
	C_RADIUS(ufd) = radius
	C_PV(ufd) = 1

	call pl_regionrop (pl, pe_ucircle, ufd, y1, y2, rop)

	call sfree (sp)
end


# PE_UCIRCLE -- Regionrop ufcn for a circle (circular region), clipped at
# the borders of the mask.

bool procedure pe_ucircle (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

real	radius, dx, dy
pointer	pl
int	rn, axlen, x1, x1_clipped, x2, x2_clipped

begin
	pl = C_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)
	radius = C_RADIUS(ufd)

	dy = abs (C_YCEN(ufd) - y)
	if (dy < radius) {
	    dx = radius * radius - dy * dy
	    if (dx > 0.0)
		dx = sqrt (dx)
	    else
		dx = 0.0
	    x1 = int(C_XCEN(ufd) - dx)
	    x2 = int(C_XCEN(ufd) + dx)
	    x1_clipped = max(1, min (axlen, x1))
	    x2_clipped = max(x1, min (axlen, x2))
	    xs = x1_clipped
	    npix = x2_clipped - x1_clipped + 1
	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = npix
	    RL_V(rl_reg,rn) = C_PV(ufd)
	    rn = rn + 1
	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end


define	LEN_ELLDES	10
define	E_PL		Memi[$1]	# reference mask
define	E_XCEN		Memr[$1+1]	# X center of ellipse
define	E_YCEN		Memr[$1+2]	# Y center of ellipse
define	E_AA		Memr[$1+3]	# aa parameter
define	E_BB		Memr[$1+4]	# bb parameter
define	E_CC		Memr[$1+5]	# cc parameter
define	E_FF		Memr[$1+6]	# ff paramater
define	E_DXMAX		Memr[$1+7]	# the maximum x offset
define	E_DYMAX		Memr[$1+8]	# the maximum x offset
define	E_PV		Memi[$1+9]	# pixel value

# PE_ELLIPSE -- Rasterop between an elliptical region as source and an existing
# mask as destination.  It is not necessary for the center of the ellipse to
# be inside the mask; if it is outside, the boundary of the ellipse will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified. These routines are
# a modification of the ones in plio$plcircle. The main difference is
# that the x, y, radius parameters are initially set to real numbers not
# integers.

procedure pe_ellipse (pl, x, y, radius, ratio, theta, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of ellipse
real	radius			#I semi-major axis of ellipse
real	ratio			#I the ratio semi-minor / semi-major axes
real	theta			#I position angle in degrees
int	rop			#I rasterop

real	aa, bb, cc, ff, dx, dy
real	y1r, y2r, x1r, x2r, r2
int	y1, y2
pointer	sp, ufd
extern	pe_uellipse()

begin
	# Not sure why we need to call this routine here.
	#call plvalid (pl)

	# Get ellipse parameters.
	call me_ellgeom (radius, ratio, theta, aa, bb, cc, ff)
	r2 = radius * radius
	dx = ff / (aa - bb * bb / 4.0 / cc)
	if (dx > 0.0)
	    dx = sqrt (dx)
	else
	    dx = 0.0
	dy = ff / (cc - bb * bb / 4.0 / aa)
	if (dy > 0.0)
	    dy = sqrt (dy)
	else
	    dy = 0.0

	# Test the line and column limits.
	y1r = y - dy
	y2r = y + dy
	x1r = x - dx
	x2r = x + dx
	if ((y2r < 0.5) || (y1r > PL_AXLEN(pl,2) + 0.5))
	    return
	if ((x2r < 0.5) || (x1r > PL_AXLEN(pl,1) + 0.5))
	    return

	call smark (sp)
	call salloc (ufd, LEN_ELLDES, TY_STRUCT)
	y1 = max ( 1, min (PL_AXLEN(pl,2), int(y1r)))
	y2 = max (y1, min (PL_AXLEN(pl,2), int(y2r)))

	E_PL(ufd) = pl
	E_XCEN(ufd) = x
	E_YCEN(ufd) = y
	E_DXMAX(ufd) = dx
	E_DYMAX(ufd) = dy
	E_AA(ufd) = aa / r2
	E_BB(ufd) = bb  / r2
	E_CC(ufd) = cc / r2
	E_FF(ufd) = ff / r2
	E_PV(ufd) = 1

	call pl_regionrop (pl, pe_uellipse, ufd, y1, y2, rop)

	call sfree (sp)
end


# PE_UELLIPSE -- Regionrop ufcn for an ellipse (elliptical region), clipped at
# the borders of the mask.

bool procedure pe_uellipse (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

real	dy, dy2, ady, bb, cc, discr, dx1, dx2
pointer	pl
int	rn, axlen, x1, x1_clipped, x2, x2_clipped

begin
	pl = E_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)

	dy = y - E_YCEN(ufd)
	dy2 = dy * dy
	ady = abs (dy)
	bb = E_BB(ufd) * dy
	cc = E_CC(ufd) * dy2
	if (ady < E_DYMAX(ufd)) {
	    discr = bb * bb - 4.0 * E_AA(ufd) * (cc - E_FF(ufd))
	    if (discr > 0.0)
		discr = sqrt (discr)
	    else
		discr = 0.0
	    dx1 = (-bb - discr) / 2.0 / E_AA(ufd)
	    dx2 = (-bb + discr) / 2.0 / E_AA(ufd)
	    x1 = int(E_XCEN(ufd) + min (dx1, dx2))
	    x2 = int(E_XCEN(ufd) + max (dx1, dx2))
	    x1_clipped = max(1, min (axlen, x1))
	    x2_clipped = max(x1, min (axlen, x2))
	    xs = x1_clipped
	    npix = x2_clipped - x1_clipped + 1
	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = npix
	    RL_V(rl_reg,rn) = E_PV(ufd)
	    rn = rn + 1
	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end


define	LEN_BOXDES	6
define	B_PL		Memi[$1]	# reference mask
define	B_X1		Memr[$1+1]	# X1 lower left corner of box
define	B_Y1		Memr[$1+2]	# Y1 lower left corner of box
define	B_X2		Memr[$1+3]	# X2 upper right corner of box
define	B_Y2		Memr[$1+4]	# Y2 upper right corner of box
define	B_PV		Memi[$1+5]	# pixel value

# PE_BOX -- Rasterop between a rectangular region as source and an existing
# mask as destination.  It is not necessary for the corners of the box to
# be inside the mask; if they are outside, the boundary of the box will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified. These routines are
# a modification of the ones in plio$plbox. The main difference is
# that the x, y, radius parameters are initially set to real numbers not
# integers.

procedure pe_box (pl, x1, y1, x2, y2, rop)

pointer	pl			#I mask descriptor
real	x1, y1			#I lower left corner of box
real	x2, y2			#I upper right corner of box
int	rop			#I rasterop

pointer	sp, ufd
extern	pe_ubox()

begin
	# Not sure why we need to call this routine here.
	#call plvalid (pl)

	# Test the line and column limits.
	if ((y2 < 0.5) || (y1 > PL_AXLEN(pl,2) + 0.5))
	    return
	if ((x2 < 0.5) || (x1 > PL_AXLEN(pl,1) + 0.5))
	    return

	call smark (sp)
	call salloc (ufd, LEN_BOXDES, TY_STRUCT)

	B_PL(ufd) = pl
	B_X1(ufd) = max (1, min (PL_AXLEN(pl,1), nint(x1)))
	B_Y1(ufd) = max (1, min (PL_AXLEN(pl,2), nint(y1)))
	B_X2(ufd) = max (1, min (PL_AXLEN(pl,1), nint(x2)))
	B_Y2(ufd) = max (1, min (PL_AXLEN(pl,2), nint(y2)))
	B_PV(ufd) = 1

	call pl_regionrop (pl, pe_ubox, ufd, int(B_Y1(ufd)), int(B_Y2(ufd)),
	    rop)

	call sfree (sp)
end


# PE_UBOX -- Regionrop ufcn for an unrotated rectangle  (rectangular region),
# clipped at the borders of the mask.

bool procedure pe_ubox (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

int	rn 
bool	rl_new

begin
	rl_new = true
	rn = RL_FIRST

	if (y >= B_Y1(ufd) && y <= B_Y2(ufd)) {
	    xs = B_X1(ufd) 
	    npix = B_X2(ufd) - B_X1(ufd) + 1
	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = npix
	    RL_V(rl_reg,rn) = B_PV(ufd)
	    rn = rn + 1
	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end


# PE_RECTANGLE -- Rasterop between a rectangular region as source and an
# existing mask as destination.  It is not necessary for the center of the
# rectangle to be inside the mask; if it is outside, the boundary of the
# rectangle will be clipped to the boundary of the mask. This is a 2-dim
# operator.  If the image dimensionality is greater than two the pl_setplane
# procedure should  be called first to specify the plane to be modified.
# These routines are a modification of the ones in plio$plcircle. The main
# difference is that the x, y, radius parameters are initially set to real
# numbers not integers.

procedure pe_rectangle (pl, x, y, radius, ratio, theta, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of rectangle
real	radius			#I semi-major axis of rectangle
real	ratio			#I the ratio semi-minor / semi-major axes
real	theta			#I position angle in degrees
int	rop			#I rasterop

real	xr[4], yr[4]
int	i

begin
	# Get rectangle vertices.
	call me_rectgeom (radius, ratio, theta, xr, yr)
	do i = 1, 4 {
	    xr[i] = x + xr[i]
	    yr[i] = y + yr[i]
	}

	# Mark the polygon.
	call pe_polygon (pl, xr, yr, 4, rop)
end


# PE_VECTOR -- Rasterop between a rectangular vector region as source and an
# existing mask as destination.  It is not necessary for the center of the
# rectangle to be inside the mask; if it is outside, the boundary of the
# rectangle will be clipped to the boundary of the mask. This is a 2-dim
# operator.  If the image dimensionality is greater than two the pl_setplane
# procedure should  be called first to specify the plane to be modified.
# These routines are a modification of the ones in plio$plcircle. The main
# difference is that the x, y, radius parameters are initially set to real
# numbers not integers.

procedure pe_vector (pl, x1, y1, x2, y2, width, rop)

pointer	pl			#I mask descriptor
real	x1, y1			#I beginning point of vector
real	x2, y2			#I ending point of vector
real	width			#I width of vector
int	rop			#I rasterop

real	xr[4], yr[4]
real	xc, yc, radius, ratio, theta
int	i

begin
	# Compute the center of the rectangle.
	xc = (x1 + x2) / 2.0
	yc = (y1 + y2) / 2.0

	# Compute the semi-major axis, axis ratio, and position angle.
	radius = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2) / 2.0
	if (radius <= 0.0)
	    return
	ratio = width / radius
	theta = RADTODEG (atan2 (y2 - y1, x2 - x1))

	# Get rectangle vertices.
	call me_rectgeom (radius, ratio, theta, xr, yr)

	# Add back in the center coordinates.
	do i = 1, 4 {
	    xr[i] = xc + xr[i]
	    yr[i] = yc + yr[i]
	}

	# Mark the polygon.
	call pe_polygon (pl, xr, yr, 4, rop)
end


define	TOL		0.0001		# pixel units
define	swapi		{tempi=$2;$2=$1;$1=tempi}
define	swapr		{tempr=$2;$2=$1;$1=tempr}
define	equal		(abs($1-$2)<TOL)

define	LEN_PGONDES	7
define	P_PL		Memi[$1]	# pointer to X vector
define	P_XP		Memi[$1+1]	# pointer to X vector
define	P_YP		Memi[$1+2]	# pointer to Y vector
define	P_OO		Memi[$1+3]	# pointer to previous range list
define	P_OY		Memi[$1+4]	# y value of previous range list
define	P_NS		Memi[$1+5]	# number of line segments
define	P_PV		Memi[$1+6]	# pixel value


# PE_POLYGON -- Perform a rasterop operation on the area enclosed by a polygon
# drawn in a 2-dimensional plane of a mask.  If the dimensionality of the mask
# exceeds 2, the pl_setplane() procedure should be called first to define the
# plane of the mask to be modified.

procedure pe_polygon (pl, x, y, npts, rop)

pointer	pl			#I mask descriptor
real	x[npts]			#I polygon x-vertices
real	y[npts]			#I polygon y-vertices
int	npts			#I number of points in polygon
int	rop			#I rasterop defining operation

real	line_1r, line_2r
pointer	sp, ufd, xp, yp, oo
int	line_1, line_2, i
extern	pe_upolygon()
errchk	plvalid

begin
	# Note sure why this is called.
	#call plvalid (pl)
	if (npts < 3)
	    return

	call smark (sp)
	call salloc (ufd, LEN_PGONDES, TY_STRUCT)
	call salloc (oo, RL_FIRST + (npts+1)*3, TY_INT)
	call salloc (xp, npts + 1, TY_REAL)
	call salloc (yp, npts + 1, TY_REAL)

	# Initialize the region descriptor.
	P_PL(ufd) = pl
	P_XP(ufd) = xp
	P_YP(ufd) = yp
	P_PV(ufd) = 1
	P_OO(ufd) = oo
	P_OY(ufd) = -1
	P_NS(ufd) = npts - 1
	RLI_LEN(oo) = 0

	# Copy the user supplied polygon vertices into the descriptor,
	# normalizing the polygon in the process.

	do i = 1, npts {
	    Memr[xp+i-1] = x[i]
	    Memr[yp+i-1] = y[i]
	}

	if (abs(x[1]-x[npts]) > TOL || abs(y[1]-y[npts]) > TOL) {
	    Memr[xp+npts] = x[1]
	    Memr[yp+npts] = y[1]
	    P_NS(ufd) = npts
	}

	# Compute the range in Y in which the polygon should be drawn.
	call alimr (y, npts, line_1r, line_2r)
	line_1 = max (1, min (PL_AXLEN(pl,2), int (line_1r)))
	line_2 = max (line_1, min (PL_AXLEN(pl,2), int (line_2r)))

	call pl_regionrop (pl, pe_upolygon, ufd, line_1, line_2, rop)

	call sfree (sp)
end


# PE_UPOLYGON -- Regionrop ufcn for a general closed polygonal region.
# This a copy of pl_upolgon which contains the following editorial comment.
# Surely there must be a simpler way to code this ... I have a polygon
# routines of my own which I use in the photometry code which may be
# a bit simpler. Might replace this at some point.

bool procedure pe_upolygon (ufd, line, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	line			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O start of edit region in dst mask
int	npix			#O number of pixels affected

pointer	xp, yp, pl
bool	rl_new, cross
int	nseg, np, low, rn, i1, i2, ii, i, j
int	tempi, axlen, rl_len, p_prev, p_next
real	tempr, y, y1, y2, x1, x2, p1, p2, p_y, n_y

int	btoi()
bool	plr_equali()
define	done_ 91

begin
	pl = P_PL(ufd)
	axlen = PL_AXLEN(pl,1)
	rn = RL_FIRST
	npix = 0
	xs = 1

	nseg = P_NS(ufd)
	xp = P_XP(ufd)
	yp = P_YP(ufd)
	y  = real(line)

	# Find the point(s) of intersection of the current mask line with
	# the line segments forming the polygon.  Note that the line must
	# cross a segment to go from inside to outside or vice versa; if a
	# segment (or vertex) is merely touched it should be drawn, but it
	# is not a point of crossing.

	do i = 1, nseg {
	    # Locate next and previous line segments.
	    if (i == 1)
		p_prev = nseg
	    else
		p_prev = i - 1
	    if (i == nseg)
		p_next = 1
	    else
		p_next = i + 2

	    # Get endpoints of current segment.
	    x1 = Memr[xp+i-1];  x2 = Memr[xp+i]
	    y1 = Memr[yp+i-1];  y2 = Memr[yp+i]
	    if (y1 > y2) {
		swapr (x1, x2)
		swapr (y1, y2)
		swapi (p_next, p_prev)
	    }

	    # Does current line intersect the polygon line segment?
	    if (y > y1-TOL && y < y2+TOL) {
		p_y = Memr[yp+p_prev-1]
		n_y = Memr[yp+p_next-1]

		if (y2 - y1 > TOL) {
		    # Single point of intersection.
		    p1 = x1 + ((x2 - x1) / (y2 - y1)) * (y - y1)
		    p2 = p1

		    if (equal (p1, x1) && equal (y, y1))
			cross = ((p_y - y1) < 0)
		    else if (equal (p1, x2) && equal (y, y2))
			cross = ((n_y - y2) > 0)
		    else
			cross = true

		} else {
		    # Intersection is entire line segment.
		    p1 = x1;  p2 = x2
		    cross = (((p_y - y) * (n_y - y)) < 0)
		}

		i1 = max(1, min(axlen, nint(p1)))
		i2 = max(1, min(axlen, nint(p2)))
		if (i1 > i2)
		    swapi (i1, i2)

		np = i2 - i1 + 1
		if (np > 0) {
		    RL_X(rl_reg,rn) = i1
		    RL_N(rl_reg,rn) = np
		    RL_V(rl_reg,rn) = btoi(cross)
		    rn = rn + 1
		}
	    }
	}

	rl_len = rn - 1
	if (rl_len <= RL_FIRST)
	    goto done_

	# Sort the line intersection-segments in order of increasing X.
	do j = RL_FIRST, rl_len {
	    # Get low X value of initial segment.
	    i1 = RL_X(rl_reg,j)
	    np = RL_N(rl_reg,j)
	    i1 = min (i1, i1 + np - 1)
	    low = j

	    # Find lowest valued segment in remainder of array.
	    do i = j+1, rl_len {
		i2 = RL_X(rl_reg,i)
		np = RL_N(rl_reg,i)
		i2 = min (i2, i2 + np - 1)
		if (i2 < i1) {
		    i1 = i2
		    low = i
		}
	    }
	    
	    # Interchange the initial segment and the low segment.
	    if (low != j) {
		swapi (RL_X(rl_reg,j), RL_X(rl_reg,low))
		swapi (RL_N(rl_reg,j), RL_N(rl_reg,low))
		swapi (RL_V(rl_reg,j), RL_V(rl_reg,low))
	    }
	}

	# Combine any segments which overlap.
	rn = RL_FIRST
	do i = RL_FIRST + 1, rl_len {
	    i1 = RL_X(rl_reg,rn)
	    i2 = RL_N(rl_reg,rn) + i1 - 1
	    ii = RL_X(rl_reg,i)
	    if (ii >= i1 && ii <= i2) {
		i2 = ii + RL_N(rl_reg,i) - 1
		RL_N(rl_reg,rn) = max (RL_N(rl_reg,rn), i2 - i1 + 1)
		RL_V(rl_reg,rn) = max (RL_V(rl_reg,rn), RL_V(rl_reg,i))
	    } else {
		rn = rn + 1
		RL_X(rl_reg,rn) = RL_X(rl_reg,i)
		RL_N(rl_reg,rn) = RL_N(rl_reg,i)
		RL_V(rl_reg,rn) = RL_V(rl_reg,i)
	    }
	}
	rl_len = rn

	# Now combine successive pairs of intersections to produce the line
	# segments to be drawn.  If all points are crossing points (where the
	# image line crosses the polygon boundary) then we draw a line between
	# the first two points, then the second two points, and so on.  Points
	# where the image line touches the polygon boundary but does not cross
	# it are plotted, but are not joined with other points to make line
	# segments.

	rn = RL_FIRST
	ii = RL_FIRST

	do j = RL_FIRST, rl_len {
	    if (j <= ii && j < rl_len) {
		next

	    } else if (RL_V(rl_reg,ii) == YES) {
		# Skip a vertext that touches but does not cross.
		if (RL_V(rl_reg,j) == NO && j < rl_len)
		    next

		# Draw a line between the two crossing points.
		RL_X(rl_reg,rn) = RL_X(rl_reg,ii)
		RL_N(rl_reg,rn) = max (RL_N(rl_reg,ii),
		    RL_X(rl_reg,j) + RL_N(rl_reg,j) - RL_X(rl_reg,ii))
		RL_V(rl_reg,rn) = P_PV(ufd)
		rn = rn + 1
		ii = j + 1

	    } else {
		# Plot only the first point.
		RL_X(rl_reg,rn) = RL_X(rl_reg,ii)
		RL_N(rl_reg,rn) = RL_N(rl_reg,ii)
		RL_V(rl_reg,rn) = P_PV(ufd)
		rn = rn + 1

		if (j >= rl_len && j != ii) {
		    # Plot the second point, if and end of list.
		    RL_X(rl_reg,rn) = RL_X(rl_reg,j)
		    RL_N(rl_reg,rn) = RL_N(rl_reg,j)
		    RL_V(rl_reg,rn) = P_PV(ufd)
		    rn = rn + 1
		} else
		    ii = j
	    }
	}

done_
	# Convert the X values in the range list to be relative to the start
	# of the list.  Compute NPIX, the range in pixels spanned by the range
	# list.

	rl_len = rn - 1
	xs = RL_X(rl_reg,RL_FIRST)
	npix = RL_X(rl_reg,rl_len) + RL_N(rl_reg,rl_len) - xs

	do i = RL_FIRST, rl_len
	    RL_X(rl_reg,i) = RL_X(rl_reg,i) - xs + 1

	RL_LEN(rl_reg) = rl_len
	RL_AXLEN(rl_reg) = npix

	rl_new = true
	if (P_OY(ufd) == line - 1)
	    rl_new = !plr_equali (rl_reg, Memi[P_OO(ufd)])
	call amovi (rl_reg, Memi[P_OO(ufd)], rn - 1)
	P_OY(ufd) = line

	return (rl_new)
end


define	LEN_CANNDES	6
define	CA_PL		Memi[$1]	# reference mask
define	CA_XCEN		Memr[$1+1]	# x center of circle
define	CA_YCEN		Memr[$1+2]	# y center of circle
define	CA_RADIUS1	Memr[$1+3]	# inner radius of annulus
define	CA_RADIUS2	Memr[$1+4]	# outer radius of annulus
define	CA_PV		Memi[$1+5]	# pixel value

# PE_CANNULUS -- Rasterop between a circular annular region as source and an
# existing mask as destination.  It is not necessary for the center of the
# annulus to be inside the mask; if it is outside, the boundary of the
# annulus will be clipped to the boundary of the mask. This is a 2-dim
# operator.  If the image dimensionality is greater than two the pl_setplane
# procedure should be called first to specify the plane to be modified. These
# routines are a modification of the ones in plio$plcircle. The main difference
# is that the x, y, radius1, radius2, parameters are initially set to real
# numbers not integers.

procedure pe_cannulus (pl, x, y, radius1, radius2, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of circular annulus
real	radius1			#I inner radius of circular annulus
real	radius2			#I outer radius of circular annulus
int	rop			#I rasterop

real	y1r, y2r, x1r, x2r
int	y1, y2
pointer	sp, ufd
extern	pe_ucannulus()

begin
	# Not sure why we need to call this routine here
	#call plvalid (pl)

	# The outer annulus must be greater than or equal to the inner annulus
	if (radius2 < radius1)
	    return

	# Test image limits.
	y1r = y - radius2
	y2r = y + radius2
	if ((y2r < 0.5) || (y1r > (PL_AXLEN(pl,2) + 0.5)))
	    return
	x1r = x - radius2
	x2r = x + radius2
	if ((x2r < 0.5) || (x1r > (PL_AXLEN(pl,1) + 0.5)))
	    return

	call smark (sp)
	call salloc (ufd, LEN_CANNDES, TY_STRUCT)

	y1 = max ( 1, min (PL_AXLEN(pl,2), int(y1r)))
	y2 = max (y1, min (PL_AXLEN(pl,2), int(y2r)))

	CA_PL(ufd) = pl
	CA_XCEN(ufd) = x
	CA_YCEN(ufd) = y
	CA_RADIUS1(ufd) = radius1
	CA_RADIUS2(ufd) = radius2
	CA_PV(ufd) = 1

	call pl_regionrop (pl, pe_ucannulus, ufd, y1, y2, rop)

	call sfree (sp)
end


# PE_UCANNULUS -- Regionrop ufcn for a circular annulus clipped at the borders
# of the mask.

bool procedure pe_ucannulus (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

real	radius1, radius2, dx, dy
pointer	pl
int	rn, axlen, x1o, x1o_clipped, x2o, x2o_clipped, x1i, x1i_clipped
int	x2i, x2i_clipped

begin
	pl = CA_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)
	radius1 = CA_RADIUS1(ufd)
	radius2 = CA_RADIUS2(ufd)

	dy = abs (CA_YCEN(ufd) - y)
	if (dy < radius2) {
	    dx = radius2 * radius2 - dy * dy
	    if (dx > 0.0)
		dx = sqrt (dx)
	    else
		dx = 0.0
	    x1o = int (CA_XCEN(ufd) - dx)
	    x2o = int (CA_XCEN(ufd) + dx)
	    x1o_clipped = max(1, min(axlen, x1o))
	    x2o_clipped = max(1, min(axlen, x2o))
	    xs = x1o_clipped
	    if (dy < radius1) {
	        dx = radius1 * radius1 - dy * dy
		if (dx > 0.0)
		    dx = sqrt (dx)
		else
		    dx = 0.0
	        x1i = int (CA_XCEN(ufd) - dx)
	        x2i = int (CA_XCEN(ufd) + dx)
	        x1i_clipped = max(1, min (axlen, x1i))
	        x2i_clipped = max(1, min (axlen, x2i))
	        RL_X(rl_reg,rn) = 1
	        RL_N(rl_reg,rn) = x1i_clipped - x1o_clipped + 1
	        RL_V(rl_reg,rn) = CA_PV(ufd)
	        rn = rn + 1
	        RL_X(rl_reg,rn) = x2i_clipped - x1o_clipped + 1 
	        RL_N(rl_reg,rn) = x2o_clipped - x2i_clipped + 1
	        RL_V(rl_reg,rn) = CA_PV(ufd)
	        rn = rn + 1
		npix = x2o_clipped - x1o_clipped + 1
	    } else {
	        RL_X(rl_reg,rn) = 1
	        RL_N(rl_reg,rn) = x2o_clipped - x1o_clipped + 1
	        RL_V(rl_reg,rn) = CA_PV(ufd)
	        npix = RL_N(rl_reg,rn)
	        rn = rn + 1
	    }
	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end

define	LEN_EANNDES	16
define	EA_PL		Memi[$1]	# reference mask
define	EA_XCEN		Memr[$1+1]	# x center of ellipse
define	EA_YCEN		Memr[$1+2]	# y center of ellipse
define	EA_AA1		Memr[$1+3]	# aa parameter for inner ellipse
define	EA_BB1		Memr[$1+4]	# bb parameter for inner ellipse
define	EA_CC1		Memr[$1+5]	# cc parameter for inner ellipse
define	EA_FF1		Memr[$1+6]	# ff parameter for inner ellipse
define	EA_DXMAX1	Memr[$1+7]      # max dx value for inner ellipse
define	EA_DYMAX1	Memr[$1+8]      # max dy value for inner ellipse
define	EA_AA2		Memr[$1+9]	# aa parameter for outer ellipse
define	EA_BB2		Memr[$1+10]	# bb parameter for outer ellipse
define	EA_CC2		Memr[$1+11]	# cc parameter for outer ellipse
define	EA_FF2		Memr[$1+12]	# ff parameter for outer ellipse
define	EA_DXMAX2	Memr[$1+13]     # max dx value for outer ellipse
define	EA_DYMAX2	Memr[$1+14]     # max dy value for outer ellipse
define	EA_PV		Memi[$1+15]	# pixel value

# PE_EANNULUS -- Rasterop between an elliptical annular region as source and an
# existing mask as destination.  It is not necessary for the center of the
# annulus to be inside the mask; if it is outside, the boundary of the
# annulus will be clipped to the boundary of the mask. This is a 2-dim
# operator.  If the image dimensionality is greater than two the pl_setplane
# procedure should be called first to specify the plane to be modified. These
# routines are a modification of the ones in plio$plcircle. The main difference
# is that the x, y, radius1, radius2, parameters are initially set to real
# numbers not integers.

procedure pe_eannulus (pl, x, y, radius1, radius2, ratio, theta, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of circular annulus
real	radius1			#I inner radius of circular annulus
real	radius2			#I outer radius of circular annulus
real	ratio			#I the semi-minor / semi-major axis ratio
real	theta			#I the position angle in degrees
int	rop			#I rasterop

real	aa, bb, cc, ff, r2, dx, dy
real	y1r, y2r, x1r, x2r
int	y1, y2
pointer	sp, ufd
extern	pe_ueannulus()

begin
	# Not sure why we need to call this routine here
	#call plvalid (pl)

	# The outer annulus must be greater than or equal to the inner annulus
	if (radius2 < radius1)
	    return

	# Get the outer ellipse parameters.
	call me_ellgeom (radius2, ratio, theta, aa, bb, cc, ff)
	r2 = radius2 * radius2
	dx = ff / (aa - bb * bb / 4.0 / cc)
	if (dx > 0.0)
	    dx = sqrt (dx)
	else
	    dx = 0.0
	dy = ff / (cc - bb * bb / 4.0 / aa)
	if (dy > 0.0)
	    dy = sqrt (dy)
	else
	    dy = 0.0

	# Test image limits.
	y1r = y - dy
	y2r = y + dy
	if ((y2r < 0.5) || (y1r > (PL_AXLEN(pl,2) + 0.5)))
	    return
	x1r = x - dx
	x2r = x + dx
	if ((x2r < 0.5) || (x1r > (PL_AXLEN(pl,1) + 0.5)))
	    return

	call smark (sp)
	call salloc (ufd, LEN_EANNDES, TY_STRUCT)

	EA_PL(ufd) = pl
	EA_XCEN(ufd) = x
	EA_YCEN(ufd) = y
	EA_AA2(ufd) = aa / r2
	EA_BB2(ufd) = bb / r2
	EA_CC2(ufd) = cc / r2
	EA_FF2(ufd) = ff / r2
	EA_DXMAX2(ufd) = dx
	EA_DYMAX2(ufd) = dy
	EA_PV(ufd) = 1

	# Get the inner  ellipse parameters.
	call me_ellgeom (radius1, ratio, theta, aa, bb, cc, ff)
	r2 = radius1 * radius1
	dx = ff / (aa - bb * bb / 4.0 / cc)
	if (dx > 0.0)
	    dx = sqrt (dx)
	else
	    dx = 0.0
	dy = ff / (cc - bb * bb / 4.0 / aa)
	if (dy > 0.0)
	    dy = sqrt (dy)
	else
	    dy = 0.0

	EA_AA1(ufd) = aa / r2
	EA_BB1(ufd) = bb / r2
	EA_CC1(ufd) = cc / r2
	EA_FF1(ufd) = ff / r2
	EA_DXMAX1(ufd) = dx
	EA_DYMAX1(ufd) = dy

	y1 = max ( 1, min (PL_AXLEN(pl,2), int(y1r)))
	y2 = max (y1, min (PL_AXLEN(pl,2), int(y2r)))
	call pl_regionrop (pl, pe_ueannulus, ufd, y1, y2, rop)

	call sfree (sp)
end


# PE_UEANNULUS -- Regionrop ufcn for a circular annulus clipped at the borders
# of the mask.

bool procedure pe_ueannulus (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

real	dy, dy2, ady, bb2, cc2, bb1, cc1, discr, dx1, dx2
pointer	pl
int	rn, axlen, x1o, x1o_clipped, x2o, x2o_clipped, x1i, x1i_clipped
int	x2i, x2i_clipped

begin
	pl = EA_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)

	dy = y - EA_YCEN(ufd)
	dy2 = dy * dy
	ady = abs (dy)
	bb2 = EA_BB2(ufd) * dy
	cc2 = EA_CC2(ufd) * dy2
	bb1 = EA_BB1(ufd) * dy
	cc1 = EA_CC1(ufd) * dy2

	if (ady < EA_DYMAX2(ufd)) {
	    discr = bb2 * bb2 - 4.0 * EA_AA2(ufd) * (cc2 - EA_FF2(ufd))
	    if (discr > 0.0)
		discr = sqrt (discr)
	    else
		discr = 0.0
	    dx1 = (-bb2 - discr) / 2.0 / EA_AA2(ufd)
	    dx2 = (-bb2 + discr) / 2.0 / EA_AA2(ufd)
	    x1o = EA_XCEN(ufd) + min (dx1, dx2)
	    x2o = EA_XCEN(ufd) + max (dx1, dx2)
	    x1o_clipped = max(1, min(axlen, x1o))
	    x2o_clipped = max(1, min(axlen, x2o))
	    xs = x1o_clipped
	    if (ady < EA_DYMAX1(ufd)) {
	        discr = bb1 * bb1 - 4.0 * EA_AA1(ufd) * (cc1 - EA_FF1(ufd))
	        if (discr > 0.0)
		    discr = sqrt (discr)
	        else
		    discr = 0.0
	        dx1 = (-bb1 - discr) / 2.0 / EA_AA1(ufd)
	        dx2 = (-bb1 + discr) / 2.0 / EA_AA1(ufd)
	        x1i = EA_XCEN(ufd) + min (dx1, dx2)
	        x2i = EA_XCEN(ufd) + max (dx1, dx2)
	        x1i_clipped = max(1, min (axlen, x1i))
	        x2i_clipped = max(1, min (axlen, x2i))
	        RL_X(rl_reg,rn) = 1
	        RL_N(rl_reg,rn) = x1i_clipped - x1o_clipped + 1
	        RL_V(rl_reg,rn) = EA_PV(ufd)
	        rn = rn + 1
	        RL_X(rl_reg,rn) = x2i_clipped - x1o_clipped + 1 
	        RL_N(rl_reg,rn) = x2o_clipped - x2i_clipped + 1
	        RL_V(rl_reg,rn) = EA_PV(ufd)
	        rn = rn + 1
		npix = x2o_clipped - x1o_clipped + 1
	    } else {
	        RL_X(rl_reg,rn) = 1
	        RL_N(rl_reg,rn) = x2o_clipped - x1o_clipped + 1
	        RL_V(rl_reg,rn) = EA_PV(ufd)
	        npix = RL_N(rl_reg,rn)
	        rn = rn + 1
	    }
	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end


define	LEN_RANNDES	7
define	RA_PL		Memi[$1]	# the mask descriptor
define	RA_IXP		Memi[$1+1]	# pointer to inner polygon X vector
define	RA_IYP		Memi[$1+2]	# pointer to inner Y polygon vector
define	RA_OXP		Memi[$1+3]	# pointer to outer X polygon vector
define	RA_OYP		Memi[$1+4]	# pointer to outer Y polygon vector
define	RA_NVER		Memi[$1+5]	# number of vertices
define	RA_PV		Memi[$1+6]	# mask pixel value


# PE_RANNULUS -- Perform a rasterop operation on the area enclosed by a
# rectangular annulus drawn in a 2-dimensional plane of a mask. If the
# dimensionality of the mask exceeds 2, the pl_setplane() procedure should be
# called first to define the plane of the mask to be modified.

procedure pe_rannulus (pl, x, y, radius1, radius2, ratio, theta, rop)

pointer	pl			#I mask descriptor
real	x, y			#I the center of the rectangular annulus
real	radius1, radius2	#I inner and outer semi-major axes
real	ratio			#I ratio of the semi-minor / semi-major axes
real	theta			#I position angle
int	rop			#I rasterop defining operation

real	line_1r, line_2r
pointer	sp, ufd, ixp, iyp, oxp, oyp
int	line_1, line_2, i
extern	pe_uarect()
errchk	plvalid

begin
	# Note sure why this is called.
	#call plvalid (pl)

	# Initialize the
	call smark (sp)
	call salloc (ufd, LEN_RANNDES, TY_STRUCT)
	call salloc (ixp, 5, TY_REAL)
	call salloc (iyp, 5, TY_REAL)
	call salloc (oxp, 5, TY_REAL)
	call salloc (oyp, 5, TY_REAL)

	# Copy and close the inner polygon.
	call me_rectgeom (radius1, ratio, theta, Memr[ixp], Memr[iyp])
	do i = 1, 4 {
	    Memr[ixp+i-1] = Memr[ixp+i-1] + x
	    Memr[iyp+i-1] = Memr[iyp+i-1] + y
	}
	Memr[ixp+4] = Memr[ixp]
	Memr[iyp+4] = Memr[iyp]

	# Create and close the outer polygon.
	call me_rectgeom (radius2, ratio, theta, Memr[oxp], Memr[oyp])
	do i = 1, 4 {
	    Memr[oxp+i-1] = Memr[oxp+i-1] + x
	    Memr[oyp+i-1] = Memr[oyp+i-1] + y
	}
	Memr[oxp+4] = Memr[oxp]
	Memr[oyp+4] = Memr[oyp]

	# Compute the range in X in which the polygon should be drawn
	# and reject polygons that are off the image.
	call alimr (Memr[oxp], 4, line_1r, line_2r)
	line_1 = max (1, min (PL_AXLEN(pl,1), int (line_1r)))
	line_2 = max (line_1, min (PL_AXLEN(pl,1), int (line_2r)))
	if (line_2 < 1 || line_1 > PL_AXLEN(pl,1)) {
	    call sfree (sp)
	    return
	}

	# Compute the range in Y in which the polygon should be drawn
	# and reject polygons that are off the image.
	call alimr (Memr[oyp], 4, line_1r, line_2r)
	line_1 = max (1, min (PL_AXLEN(pl,2), int (line_1r)))
	line_2 = max (line_1, min (PL_AXLEN(pl,2), int (line_2r)))
	if (line_2 < 1 || line_1 > PL_AXLEN(pl,2)) {
	    call sfree (sp)
	    return
	}

	# Initialize the region descriptor.
	RA_PL(ufd) = pl
	RA_IXP(ufd) = ixp
	RA_IYP(ufd) = iyp
	RA_OXP(ufd) = oxp
	RA_OYP(ufd) = oyp
	RA_NVER(ufd) = 4
	RA_PV(ufd) = 1

	call pl_regionrop (pl, pe_uarect, ufd, line_1, line_2, rop)

	call sfree (sp)
end


# PE_UARECT -- Compute the intersection of an image line and a rectangular
# polygonal annulus and define the region to be masked.

bool procedure pe_uarect (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I the region descriptor structure
int	y			#I the current line
int	rl_reg[3,ARB]		#O the output regions list
int	xs			#O the starting x value
int	npix			#O the number of pixels affected

real	lx, ld
pointer	sp, work1, work2, oxintr, ixintr, pl
int	j, jj, rn, onintr, inintr, ix1, ix2, ox1, ox2, ibegin, iend, jx1, jx2
int	me_pyclip()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (work1, RA_NVER(ufd) + 1, TY_REAL)
	call salloc (work2, RA_NVER(ufd) + 1, TY_REAL)
	call salloc (oxintr, RA_NVER(ufd) + 1, TY_REAL)
	call salloc (ixintr, RA_NVER(ufd) + 1, TY_REAL)

	# Initialize.
	pl = RA_PL(ufd)
	rn = RL_FIRST
	lx = PL_AXLEN(pl,1)
	ld = y

	# Find the intersection of the outer polygon with the image line.
	onintr = me_pyclip (Memr[RA_OXP(ufd)], Memr[RA_OYP(ufd)], Memr[work1],
	    Memr[work2], Memr[oxintr], RA_NVER(ufd) + 1, lx, ld)
	call asrtr (Memr[oxintr], Memr[oxintr], onintr)

	if (onintr > 0) {

	    # Find the intersection of the inner polygon with the image line.
	    inintr = me_pyclip (Memr[RA_IXP(ufd)], Memr[RA_IYP(ufd)],
	        Memr[work1], Memr[work2], Memr[ixintr], RA_NVER(ufd) + 1,
		lx, ld)
	    call asrtr (Memr[ixintr], Memr[ixintr], inintr)

	    # Create the region list.
	    xs = max (1, min (int(Memr[oxintr]), PL_AXLEN(pl,1)))
	    if (inintr <= 0) {
		do j = 1, onintr, 2 {
		    ox1 = max (1, min (int(Memr[oxintr+j-1]), PL_AXLEN(pl,1)))
		    ox2 = max (ox1, min (int(Memr[oxintr+j]), PL_AXLEN(pl,1)))
		    RL_X(rl_reg,rn) = ox1 - xs + 1
		    RL_N(rl_reg,rn) = ox2 - ox1 + 1
		    RL_V(rl_reg,rn) = RA_PV(ufd)
		    rn = rn + 1
		}
		npix = RL_X(rl_reg, rn-1) + RL_N(rl_reg,rn-1) - 1
	    } else {
		do j = 1, onintr, 2 {
		    ox1 = max (1, min (int(Memr[oxintr+j-1]), PL_AXLEN(pl,1)))
		    ox2 = max (ox1, min (int(Memr[oxintr+j]), PL_AXLEN(pl,1)))
		    do jj = 1, inintr, 2 {
			ix1 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			if (ix1 > ox1 && ix1 < ox2) {
			    ibegin = jj
			    break
			}
			
		    }
		    do jj = inintr, 1, -2 {
			ix2 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			if (ix2 > ox1 && ix2 < ox2) {
			    iend = jj
			    break
			}
		    }
		    RL_X(rl_reg,rn) = ox1 - xs + 1
		    RL_N(rl_reg,rn) = ix1 - ox1 + 1
		    RL_V(rl_reg,rn) = RA_PV(ufd)
		    rn = rn + 1
		    do jj = ibegin + 1, iend - 1, 2 {
			jx1 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			jx2 = max (jx1, min (int(Memr[ixintr+jj]),
			    PL_AXLEN(pl,1)))
		        RL_X(rl_reg,rn) = jx1 - xs + 1
		        RL_N(rl_reg,rn) = jx2 - jx1 + 1
		        RL_V(rl_reg,rn) = RA_PV(ufd)
			rn = rn + 1
		    }
		    RL_X(rl_reg,rn) = ix2 - xs + 1
		    RL_N(rl_reg,rn) = ox2 - ix2 + 1
		    RL_V(rl_reg,rn) = RA_PV(ufd)
		    rn = rn + 1

		}
		npix = RL_X(rl_reg, rn-1) + RL_N(rl_reg,rn-1) - 1
	    }

	} else {
	    xs = 1
	    npix = 0
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	call sfree (sp)

	return (true)
end


define	LEN_PAGONDES	7
define	PA_PL		Memi[$1]	# the mask descriptor
define	PA_IXP		Memi[$1+1]	# pointer to inner polygon X vector
define	PA_IYP		Memi[$1+2]	# pointer to inner Y polygon vector
define	PA_OXP		Memi[$1+3]	# pointer to outer X polygon vector
define	PA_OYP		Memi[$1+4]	# pointer to outer Y polygon vector
define	PA_NVER		Memi[$1+5]	# number of vertices
define	PA_PV		Memi[$1+6]	# mask pixel value


# PE_APOLYGON -- Perform a rasterop operation on the area enclosed by a
# polygonal annulus drawn in a 2-dimensional plane of a mask. If the
# dimensionality of the mask exceeds 2, the pl_setplane() procedure should be
# called first to define the plane of the mask to be modified.

procedure pe_apolygon (pl, width, x, y, npts, rop)

pointer	pl			#I mask descriptor
real	width			#I width of the polygonal annulus
real	x[npts]			#I the inner polygon x-vertices
real	y[npts]			#I outer polygon y-vertices
int	npts			#I number of points in polygon
int	rop			#I rasterop defining operation

real	line_1r, line_2r
pointer	sp, ufd, ixp, iyp, oxp, oyp
int	line_1, line_2, i
extern	pe_uapolygon()
errchk	plvalid

begin
	# Note sure why this is called.
	#call plvalid (pl)
	if (npts < 3)
	    return

	# Initialize the
	call smark (sp)
	call salloc (ufd, LEN_PAGONDES, TY_STRUCT)
	call salloc (ixp, npts + 1, TY_REAL)
	call salloc (iyp, npts + 1, TY_REAL)
	call salloc (oxp, npts + 1, TY_REAL)
	call salloc (oyp, npts + 1, TY_REAL)

	# Copy and close the inner polygon.
	do i = 1, npts {
	    Memr[ixp+i-1] = x[i]
	    Memr[iyp+i-1] = y[i]
	}
	Memr[ixp+npts] = x[1]
	Memr[iyp+npts] = y[1]

	# Create and close the outer polygon.
	call me_pyexpand (Memr[ixp], Memr[iyp], Memr[oxp], Memr[oyp],
	    npts, width)
	Memr[oxp+npts] = Memr[oxp]
	Memr[oyp+npts] = Memr[oyp]

	# Compute the range in X in which the polygon should be drawn
	# and reject polygons that are off the image.
	call alimr (Memr[oxp], npts, line_1r, line_2r)
	line_1 = max (1, min (PL_AXLEN(pl,1), int (line_1r)))
	line_2 = max (line_1, min (PL_AXLEN(pl,1), int (line_2r)))
	if (line_2 < 1 || line_1 > PL_AXLEN(pl,1)) {
	    call sfree (sp)
	    return
	}

	# Compute the range in Y in which the polygon should be drawn
	# and reject polygons that are off the image.
	call alimr (Memr[oyp], npts, line_1r, line_2r)
	line_1 = max (1, min (PL_AXLEN(pl,2), int (line_1r)))
	line_2 = max (line_1, min (PL_AXLEN(pl,2), int (line_2r)))
	if (line_2 < 1 || line_1 > PL_AXLEN(pl,2)) {
	    call sfree (sp)
	    return
	}

	# Initialize the region descriptor.
	PA_PL(ufd) = pl
	PA_IXP(ufd) = ixp
	PA_IYP(ufd) = iyp
	PA_OXP(ufd) = oxp
	PA_OYP(ufd) = oyp
	PA_NVER(ufd) = npts
	PA_PV(ufd) = 1

	call pl_regionrop (pl, pe_uapolygon, ufd, line_1, line_2, rop)

	call sfree (sp)
end


# PE_UAPOLYGON -- Compute the intersection of an image line and the polygonal
# annulus and define the region to be masked.

bool procedure pe_uapolygon (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I the region descriptor structure
int	y			#I the current line
int	rl_reg[3,ARB]		#O the output regions list
int	xs			#O the starting x value
int	npix			#O the number of pixels affected

real	lx, ld
pointer	sp, work1, work2, oxintr, ixintr, pl
int	j, jj, rn, onintr, inintr, ix1, ix2, ox1, ox2, ibegin, iend, jx1, jx2
int	me_pyclip()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (work1, PA_NVER(ufd) + 1, TY_REAL)
	call salloc (work2, PA_NVER(ufd) + 1, TY_REAL)
	call salloc (oxintr, PA_NVER(ufd) + 1, TY_REAL)
	call salloc (ixintr, PA_NVER(ufd) + 1, TY_REAL)

	# Initialize.
	pl = PA_PL(ufd)
	rn = RL_FIRST
	lx = PL_AXLEN(pl,1)
	ld = y

	# Find the intersection of the outer polygon with the image line.
	onintr = me_pyclip (Memr[PA_OXP(ufd)], Memr[PA_OYP(ufd)], Memr[work1],
	    Memr[work2], Memr[oxintr], PA_NVER(ufd) + 1, lx, ld)
	call asrtr (Memr[oxintr], Memr[oxintr], onintr)

	if (onintr > 0) {

	    # Find the intersection of the inner polygon with the image line.
	    inintr = me_pyclip (Memr[PA_IXP(ufd)], Memr[PA_IYP(ufd)],
	        Memr[work1], Memr[work2], Memr[ixintr], PA_NVER(ufd) + 1,
		lx, ld)
	    call asrtr (Memr[ixintr], Memr[ixintr], inintr)

	    # Create the region list.
	    xs = max (1, min (int(Memr[oxintr]), PL_AXLEN(pl,1)))
	    if (inintr <= 0) {
		do j = 1, onintr, 2 {
		    ox1 = max (1, min (int(Memr[oxintr+j-1]), PL_AXLEN(pl,1)))
		    ox2 = max (ox1, min (int(Memr[oxintr+j]), PL_AXLEN(pl,1)))
		    RL_X(rl_reg,rn) = ox1 - xs + 1
		    RL_N(rl_reg,rn) = ox2 - ox1 + 1
		    RL_V(rl_reg,rn) = PA_PV(ufd)
		    rn = rn + 1
		}
		npix = RL_X(rl_reg, rn-1) + RL_N(rl_reg,rn-1) - 1
	    } else {
		do j = 1, onintr, 2 {
		    ox1 = max (1, min (int(Memr[oxintr+j-1]), PL_AXLEN(pl,1)))
		    ox2 = max (ox1, min (int(Memr[oxintr+j]), PL_AXLEN(pl,1)))
		    do jj = 1, inintr, 2 {
			ix1 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			if (ix1 > ox1 && ix1 < ox2) {
			    ibegin = jj
			    break
			}
			
		    }
		    do jj = inintr, 1, -2 {
			ix2 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			if (ix2 > ox1 && ix2 < ox2) {
			    iend = jj
			    break
			}
		    }
		    RL_X(rl_reg,rn) = ox1 - xs + 1
		    RL_N(rl_reg,rn) = ix1 - ox1 + 1
		    RL_V(rl_reg,rn) = PA_PV(ufd)
		    rn = rn + 1
		    do jj = ibegin + 1, iend - 1, 2 {
			jx1 = max (1, min (int(Memr[ixintr+jj-1]),
			    PL_AXLEN(pl,1)))
			jx2 = max (jx1, min (int(Memr[ixintr+jj]),
			    PL_AXLEN(pl,1)))
		        RL_X(rl_reg,rn) = jx1 - xs + 1
		        RL_N(rl_reg,rn) = jx2 - jx1 + 1
		        RL_V(rl_reg,rn) = PA_PV(ufd)
			rn = rn + 1
		    }
		    RL_X(rl_reg,rn) = ix2 - xs + 1
		    RL_N(rl_reg,rn) = ox2 - ix2 + 1
		    RL_V(rl_reg,rn) = PA_PV(ufd)
		    rn = rn + 1

		}
		npix = RL_X(rl_reg, rn-1) + RL_N(rl_reg,rn-1) - 1
	    }

	} else {
	    xs = 1
	    npix = 0
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	call sfree (sp)

	return (true)
end


define	LEN_COLSDES	4
define	L_PL		Memi[$1]	# reference mask
define	L_RANGES	Memi[$1+1]	# pointer to the ranges	
define	L_NRANGES	Memi[$1+2]	# the number of ranges
define	L_XS		Memi[$1+3]	# the starting x coordinate value
define	L_NPIX		Memi[$1+4]	# the number of pixels value
define	L_PV		Memi[$1+5]	# pixel value

define	MAX_NRANGES	100

# PE_COLS -- Rasterop between a set of column ranges as source and an existing
# mask as destination.  It is not necessary for the ranges to  be inside the
# mask; if they are outside, the boundary of the line ranges will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified. These routines are
# a modification of the ones in plio$plcircle. The main difference is
# that the x, y, radius parameters are initially set to real numbers not
# integers.

procedure pe_cols (pl, rangestr, rop)

pointer	pl			#I mask descriptor
char	rangestr[ARB]		#I the input ranges string
int	rop			#I rasterop

int	npts, nvalues, colno, x1, x2, nregions
pointer	sp, ufd, rgptr, lineptr
int	me_decode_ranges(), me_next_number(), me_previous_number(), pl_p2ri()
extern	pe_ucols()

begin
	# Not sure why we need to call this routine here.
	#call plvalid (pl)
	npts = PL_AXLEN(pl,1)

	call smark (sp)
	call salloc (ufd, LEN_COLSDES, TY_STRUCT)
	call salloc (rgptr, 3 * MAX_NRANGES + 1, TY_INT)
	call salloc (lineptr, npts, TY_INT)

	# Decode the ranges string
	if (me_decode_ranges (rangestr, Memi[rgptr], MAX_NRANGES,
	    nvalues) == ERR) {
	    call sfree (sp)
	    return
	}

	# Get the column limits.
	x1 = INDEFI
	x2 = INDEFI
	colno = 0
	if (me_next_number (Memi[rgptr], colno) != EOF)
	    x1 = colno 
	colno = npts + 1
	if (me_previous_number (Memi[rgptr], colno) != EOF)
	    x2 = colno 
	if (IS_INDEFI(x1) || IS_INDEFI(x2)) {
	    call sfree (sp)
	    return
	}

	# Set the pixel values.
	call aclri (Memi[lineptr], npts)
	colno = 0
	while (me_next_number (Memi[rgptr], colno) != EOF) {
	    if (colno < 1 || colno > npts)
		next
	    Memi[lineptr+colno-1] = 1
	}

	# Convert the pixel list to a ranges list.
	nregions = pl_p2ri (Memi[lineptr], 1, Memi[rgptr], npts)

	L_PL(ufd) = pl
	L_RANGES(ufd) = rgptr
	L_NRANGES(ufd) = nregions
	L_XS(ufd) = 1
	L_NPIX(ufd) = npts
	L_PV(ufd) = 1

	# Call the regions operator.
	call pl_regionrop (pl, pe_ucols, ufd, 1, PL_AXLEN(pl,2), rop)

	call sfree (sp)
end


# PE_UCOLS -- Regionrop ufcn for a set of column ranges (column regions),
# clipped at the borders of the mask.

bool procedure pe_ucols (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

begin
	# Copy the ranges.
	call amovi (Memi[L_RANGES(ufd)], rl_reg, L_NRANGES(ufd) * 3)
	xs = L_XS(ufd)
	npix = L_NPIX(ufd)

	return (true)
end


define	LEN_LINESDES	3
define	L_PL		Memi[$1]	# reference mask
define	L_RANGES	Memi[$1+1]	# pointer to the ranges	
define	L_PV		Memi[$1+2]	# pixel value

define	MAX_NRANGES	100

# PE_LINES -- Rasterop between a set of line ranges as source and an existing
# mask as destination.  It is not necessary for the ranges to  be inside the
# mask; if they are outside, the boundary of the line ranges will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified. These routines are
# a modification of the ones in plio$plcircle. The main difference is
# that the x, y, radius parameters are initially set to real numbers not
# integers.

procedure pe_lines (pl, rangestr, rop)

pointer	pl			#I mask descriptor
char	rangestr[ARB]		#I the input ranges string
int	rop			#I rasterop

int	i, y1, y2, nvalues
pointer	sp, rgptr, ufd
int	me_decode_ranges()
bool	me_is_in_range()
extern	pe_ulines()

begin
	# Not sure why we need to call this routine here.
	#call plvalid (pl)

	call smark (sp)
	call salloc (ufd, LEN_LINESDES, TY_STRUCT)
	call salloc (rgptr, 3 * MAX_NRANGES, + 1, TY_INT)

	# Decode the ranges string
	if (me_decode_ranges (rangestr, Memi[rgptr], MAX_NRANGES,
	    nvalues) == ERR) {
	    call sfree (sp)
	    return
	}

	# Find the line limits.
	y1 = INDEFI
	y2 = INDEFI
	do i = 1, PL_AXLEN(pl,2) {
	    if (me_is_in_range (Memi[rgptr], i)) {
		y1 = i
		break
	    }
	}
	if (IS_INDEFI(y1)) {
	    call sfree (sp)
	    return
	}
	do i = PL_AXLEN(pl,2), 1, -1 {
	    if (me_is_in_range (Memi[rgptr], i)) {
		y2 = i
		break
	    }
	}
	if (IS_INDEFI(y2)) {
	    call sfree (sp)
	    return
	}

	L_PL(ufd) = pl
	L_RANGES(ufd) = rgptr
	L_PV(ufd) = 1

	call pl_regionrop (pl, pe_ulines, ufd, y1, y2, rop)

	call sfree (sp)
end


# PE_ULINES -- Regionrop ufcn for a set of lines ranges (line regions),
# clipped at the borders of the mask.

bool procedure pe_ulines (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

pointer	pl
int	rn, axlen
bool	me_is_in_range()

begin
	pl = L_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)

	if (me_is_in_range (Memi[L_RANGES(ufd)], y))  {
	    xs = 1
	    npix = axlen
	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = axlen
	    RL_V(rl_reg,rn) = L_PV(ufd)
	    rn = rn + 1
	} else {
	    xs = 1
	    npix = 0
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end


define	SMALL_NUMBER	1.0e-24

# PE_PIE -- Determine which pixels are inside a pie shaped wedge that
# intersects the image boundaries. 

procedure pe_pie (pl, xc, yc, angle1, angle2, rop)

pointer	pl			#I the pixel mask descriptor
real	xc, yc			#I the center of the wedge
real	angle1, angle2		#I the wedge angles
int	rop			#I the mask raster op

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
	    return
	}
	if (sweep < 0.0)
	    sweep = sweep + 360.0

	# Get the second vertext by computing the intersection of the
	# first ray with the image boundaries.
	intrcpt1 = me_pie_intercept (PL_AXLEN(pl,1), PL_AXLEN(pl,2), xc, yc,
	    angle1, vx[2], vy[2])

	# Compute the second intercept.
	intrcpt2 = me_pie_intercept (PL_AXLEN(pl,1), PL_AXLEN(pl,2), xc, yc,
	    angle2, x2, y2)

	# If angles intercept same side and slice is between them, no corners
	# else, mark corners until reaching side with second angle intercept.
	count = 3
	if ((intrcpt1 != intrcpt2) || (sweep > 180.0)) {
	    repeat {
		intrcpt1 = me_corner_vertex (intrcpt1, PL_AXLEN(pl,1),
		    PL_AXLEN(pl,2), vx[count], vy[count])
		count = count + 1
	    } until (intrcpt1 == intrcpt2)
	}

	# Set last vertex.
	vx[count] = x2
	vy[count] = y2

	# Fill in the polygon
	call pe_polygon (pl, vx, vy, count, rop)
end

include <math.h>
include <plset.h>
include <plio.h>
include "peregfuncs.h"


# PE_POINT -- Rasterop between a point region as source and an existing
# mas as destination.

procedure pe_point (pl, x, y, rop)

pointer	pl			#I mask descriptor
real	x,y			#I center coords of circle
int	rop			#I rasterop

begin
	call pl_point (pl, nint(x), nint(y), rop)
end


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
	call salloc (rgptr, 3 * MAX_NRANGES + 1, TY_INT)

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

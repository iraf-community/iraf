include <math.h>
include <plset.h>
include <plio.h>
include "peregfuncs.h"


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

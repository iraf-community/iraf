# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <gset.h>
include	"imd.h"

define	MAX_LTYPES	3	# max software line type patterns (excl. solid)
define	MAX_LSEGMENTS	4	# max line segments per pattern
define	LT_OFFSET	1	# offset to be subtracted from ltype code


# IMD_POLYLINE -- Draw a polyline.  The polyline is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted but rather defines the start of the polyline.  The remaining
# points define line segments to be drawn.

procedure imd_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pl
int	x, y
int	len_p, i
include	"imd.com"

begin
	if (npts < 2)
	    return

	len_p = npts * 2

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Update polyline attributes if necessary.
	pl = IMD_PLAP(g_kt)

	if (IMD_WIDTH(g_kt) != PL_WIDTH(pl)) {
	    call idk_linewidth (g_out, nint (GKI_UNPACKREAL(PL_WIDTH(pl))))
	    IMD_WIDTH(g_kt) = PL_WIDTH(pl)
	}
	if (IMD_COLOR(g_kt) != PL_COLOR(pl)) {
	    call imd_color (PL_COLOR(pl))
	    IMD_COLOR(g_kt) = PL_COLOR(pl)
	}

	if (PL_LTYPE(pl) == GL_CLEAR) {
	    # Ignore clear (erase) polylines.
	    ;

	} else if (PL_LTYPE(pl) != GL_SOLID) {
	    # Draw a dashed or dotted polyline of the indicated type.
	    call imd_dashline (g_out, p, npts, PL_LTYPE(pl))

	} else {
	    # Draw a solid polyline (usual case, optimized).

	    # Move to the first point.
	    x = p[1]
	    y = p[2]
	    call idk_move (g_out, x, y)

	    # Draw the polyline.
	    for (i=3;  i <= len_p;  i=i+2) {
		x = p[i]
		y = p[i+1]
		call idk_draw (g_out, x, y)
	    }
	}
end


# IMD_DASHLINE -- Draw a dashed or dotted polyline using the indicated line
# style.

procedure imd_dashline (g_out, p, npts, ltype)

int	g_out			# output file
short	p[ARB]			# the polyline points
int	npts			# number of points, i.e., (x,y) pairs
int	ltype			# desired line type

bool	penup
int	len_p, i
real	vlen, vpos, seglen, dx, dy
int	oldx, oldy, newx, newy, penx, peny
int	imd_getseg()

begin
	len_p = npts * 2

	oldx = p[1]; oldy = p[2]
	call idk_move (g_out, oldx, oldy)

	# Process each line segment in the polyline.
	do i = 3, len_p, 2 {
	    newx = p[i]
	    newy = p[i+1]

	    # Compute VLEN, the length of the polyline line segment to be
	    # drawn, VPOS, the relative position along the line segment,
	    # and DX and DY, the scale factors to be applied to VPOS to get
	    # the x and y coordinates of a point along the line segment.

	    dx = newx - oldx
	    dy = newy - oldy
	    vlen = sqrt (dx*dx + dy*dy)
	    if (vlen < 1.0)			# GKI units
		next

	    dx = dx / vlen
	    dy = dy / vlen
	    vpos = 0.0

	    # For each line segment, get segments of the line type pattern
	    # until all of the current line segment has been drawn.  The pattern
	    # wraps around indefinitely, following the polyline around the
	    # vertices with concern only for the total length traversed.

	    while (vlen - vpos >= 1.0) {
		seglen = imd_getseg (int (vlen - vpos), penup, ltype)
		if (seglen < 1.0)
		    break

		vpos = vpos + seglen
		penx = oldx + vpos * dx
		peny = oldy + vpos * dy

		if (penup)
		    call idk_move (g_out, penx, peny)
		else
		    call idk_draw (g_out, penx, peny)
	    }

	    oldx = newx
	    oldy = newy
	}
end


# IMD_GETSEG -- Get a segment of a line style pattern.  The segment extends
# from the current position in the pattern to either the next penup/pendown
# breakpoint in the pattern, or to the point MAXLEN units further along in
# the pattern.  When the end of the pattern is reached wrap around and
# duplicate the pattern indefinitely.

int procedure imd_getseg (maxlen, penup, ltype)

int	maxlen			# max length segment to be returned
bool	penup			# [out] pen up or pen down type segment?
int	ltype			# line type code

int	seglen, seg, lt
int	p_seg[MAX_LTYPES]
int	p_nseg[MAX_LTYPES]
int	p_segleft[MAX_LTYPES]
bool	p_penup[MAX_LTYPES,MAX_LSEGMENTS]
int	p_seglen[MAX_LTYPES,MAX_LSEGMENTS]
include	"ltype.dat"

begin
	lt = max (1, min (MAX_LTYPES, ltype - LT_OFFSET))
	seg = p_seg[lt]
	penup = p_penup[lt,seg]

	repeat {
	    if (maxlen < p_segleft[lt]) {
		seglen = maxlen
		p_segleft[lt] = p_segleft[lt] - seglen
	    } else {
		seglen = p_segleft[lt]
		seg = seg + 1
		if (seg > p_nseg[lt])
		    seg = 1
		p_seg[lt] = seg
		p_segleft[lt] = p_seglen[lt,seg]
	    }
	} until (seglen > 0)

	return (seglen)
end

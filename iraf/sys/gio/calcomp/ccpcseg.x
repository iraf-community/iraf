# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <gset.h>
include <mach.h>
include	"ccp.h"

# CCP_CALCSEG -- Calculate a contiguous line segment; used to return individual
# line segments under the various options of line type simulation.  (Width is
# not simulated here).  Each segment returned is actually drawable, guaranteeing
# constant-length dashes and gaps along the exact length of the input polyline. 
# Normally called by ccp_polyline.

procedure ccp_calcseg (p, npts, ltype, curpl_pt, segsize, xseg,yseg, nsegpts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	ltype			# line type; CCP_CLEAR <= ltype <= CCP_DASHDOT
int	curpl_pt		# current polyline point; input and output
int	segsize			# current segment memory size
pointer	xseg,yseg		# plotter-unit contiguous line segment, output
int	nsegpts			# number of points in segment, output

int	i, j
real	lastp_x, lastp_y, x, y, curseglen
bool	toggle

include	"ccp.com"

begin
	if (curpl_pt == 1) {	# always start line w/beginning of dash, etc.
	    lastp_x   = XTRAN(p[1])
	    lastp_y   = YTRAN(p[2])
	    curpl_pt  = curpl_pt + 1 
	    toggle    = false
	} 

	XSEG(1)   = lastp_x
	YSEG(1)   = lastp_y
	nsegpts   = 1
	curseglen = 0.0

	switch (ltype) {
	case GL_CLEAR:
	    nsegpts = 0

	case GL_DASHED:
	    # Return one contiguous polyline segment worth one dash:
	    call ccx_dash (p, npts, curpl_pt, curseglen, nsegpts, segsize,
		xseg,yseg, lastp_x,lastp_y)

	    # Now increment internal counters for gap width for next call
	    call ccx_gap (p, npts, curpl_pt, curseglen, g_dashlen + g_gaplen,
		lastp_x,lastp_y)

	case GL_DOTTED:
	    # Since we already built one point, we need only the following gap:
	    call ccx_gap (p, npts, curpl_pt, curseglen, g_gaplen,
		lastp_x,lastp_y)

	case GL_DOTDASH:
	    # Implement as dash/gap/dot/gap/:
	    if (toggle) {			# build dot/gap/
		x = lastp_x #XTRAN(p[i])
		y = lastp_y #YTRAN(p[i+1])
		nsegpts = 0
		call ccx_addsegpt (x,y, xseg,yseg, nsegpts, segsize)
		toggle = false
	        call ccx_gap (p, npts, curpl_pt, curseglen, g_gaplen, 
		    lastp_x,lastp_y)

	    } else {				# build dash/gap/
	        call ccx_dash (p, npts, curpl_pt, curseglen, nsegpts, 
		    segsize, xseg,yseg, lastp_x,lastp_y)
	        call ccx_gap (p, npts, curpl_pt, curseglen,
		    g_dashlen + g_gaplen, lastp_x,lastp_y)
		toggle = true
	    }

	default:				# solid line
	    do i = curpl_pt, npts {
		j = (i-1) * 2 + 1
		x = XTRAN(p[j])
		y = YTRAN(p[j+1])
		call ccx_addsegpt (x,y, xseg,yseg, nsegpts, segsize)
	    }
	    curpl_pt = npts
	}
end


# CCX_DASH -- Do the actual work of building a dashed line segment (no gap)

procedure ccx_dash (p, npts, curpl_pt, curseglen, cursegpt, segsize,
		    xseg,yseg, lastp_x,lastp_y)

short	p[ARB]		# Input:  points defining line
int	npts		# Input:  number of points, i.e., (x,y) pairs
int	curpl_pt	# In/Output: current polyline point
real	curseglen	# Output: length of current simulated ltype unit (._)
int	cursegpt	# Output: index of current drawable point in segment
int	segsize		# In/Output: current segment size
pointer	xseg,yseg	# Output: plotter-units, contiguous line segment
real	lastp_x,lastp_y	# Output: last point in segment (visible or invisible)

int	i
real	temppl_dis, x, y, delx, dely
real	actual_dis, rem_dashlen

include	"ccp.com"

begin
	rem_dashlen = g_dashlen

	# Build up current "dash" (may be bent any number of times).

	while (curseglen + EPSILON < g_dashlen && curpl_pt <= npts) {
	    i = (curpl_pt-1) * 2 + 1
	    x = XTRAN(p[i])
	    y = YTRAN(p[i+1])
	    temppl_dis  = DIS(lastp_x, lastp_y, x, y)
	    if (temppl_dis >= EPSILON) {
		actual_dis  = min (temppl_dis, rem_dashlen)
		rem_dashlen = rem_dashlen - actual_dis

		delx = x - lastp_x
		dely = y - lastp_y
		x = lastp_x + delx * actual_dis / temppl_dis
		y = lastp_y + dely * actual_dis / temppl_dis

		call ccx_addsegpt (x,y, xseg,yseg, cursegpt, segsize)
		curseglen = curseglen + actual_dis
		lastp_x   = XSEG(cursegpt)
		lastp_y   = YSEG(cursegpt)
	    }
	    if (curseglen + EPSILON < g_dashlen)
	        curpl_pt = curpl_pt + 1
	}
end


# CCX_GAP -- Do the actual work of building an invisible gap along original
# polyline.

procedure ccx_gap (p, npts, curpl_pt, curseglen, matchlen, lastp_x,lastp_y)

short	p[ARB]		# Input: points defining line
int	npts		# Input: number of points, i.e., (x,y) pairs
int	curpl_pt	# In/Output: current polyline point
real	curseglen	# In/Output: length of current simulated ltype unit (._)
real	matchlen	# Output: length to build curseglen up to
real	lastp_x,lastp_y	# Output: last point in segment (visible, invisible)

int	i
real	x, y, delx, dely
real	temppl_dis, actual_dis, rem_gaplen

include	"ccp.com"

begin
	rem_gaplen = g_gaplen

	# Build up current "gap" (may be bent any number of times).

	while ((curseglen + EPSILON < (matchlen)) && (curpl_pt <= npts)) {
	    i = (curpl_pt-1) * 2 + 1
	    x = XTRAN(p[i])
	    y = YTRAN(p[i+1])

	    temppl_dis = DIS(lastp_x, lastp_y, x, y)
	    if (temppl_dis >= EPSILON) {
		actual_dis = min (temppl_dis, rem_gaplen)
		rem_gaplen = rem_gaplen - actual_dis

		delx = x - lastp_x
		dely = y - lastp_y
		curseglen = curseglen + actual_dis
		lastp_x = lastp_x + delx * actual_dis / temppl_dis
		lastp_y = lastp_y + dely * actual_dis / temppl_dis
	    }
	    if (curseglen + EPSILON < matchlen)
		curpl_pt = curpl_pt + 1
	}
end


# CCX_ADDSEGPT -- add a point to the segment structure; handle memory needs

procedure ccx_addsegpt (x,y, xseg,yseg, cursegpt,segsize)

real	x,y			# point to be added to output segment
pointer	xseg,yseg		# NDC-coord contiguous line segment, output
int	cursegpt		# index of current drawable point in segment
int	segsize			# current segment size

begin
	cursegpt = cursegpt + 1

	if (cursegpt > segsize) {
	    segsize = segsize + SEGSIZE
	    call realloc (xseg, segsize, TY_REAL)
	    call realloc (yseg, segsize, TY_REAL)
	}

	XSEG(cursegpt) = x
	YSEG(cursegpt) = y
end

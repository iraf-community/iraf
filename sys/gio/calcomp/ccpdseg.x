# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>
include "ccp.h"

define	DIAGSEP	(g_plwsep / 0.8660254)	# distance to vertex of hexagon
define	PIOVER3	(PI / 3.0)
define	PIOVER6 (PI / 6.0)
define	SIN_MIN_HALFBISECTOR	0.1	# sine of minimum half-bisector


# CCP_DRAWSEG -- Draw a polyline segment, optionally simulating variable
# widths.

procedure ccp_drawseg (xseg, yseg, nsegpts, lwidth)

real	xseg[ARB]		# plotter coordinate array of contiguous points
real	yseg[ARB]
int	nsegpts			# number of pts in array
int	lwidth			# line width relative to single width

int	i, j
real	pleft_x[MAXTRACES], pleft_y[MAXTRACES]
real	pright_x[MAXTRACES], pright_y[MAXTRACES], lastp_x,lastp_y
real	ahp2p1, theta, delx,dely, dx,dy, tx,ty
real	rptheta4 ()
include	"ccp.com"
data	lastp_x/0.0/, lastp_y/0.0/

begin
	if (nsegpts < 1) 
	    return
	if (lwidth > MAXTRACES) {
	    call eprintf ("WARNING: line width > MAXTRACES in ccp_drawseg\n")
	    call eprintf ("         line width reset to %d\n")
		call pargi (MAXTRACES)
	    lwidth = MAXTRACES
	}

	if (nsegpts == 1) {					# 1 pt spcl bold

	    # Draw a single point as a hexagon lined up in the direction from 
	    # the preceding point.  Start bounding hexagon 60 degrees cc from 
	    # projection of last point drawn (0,0 initially) through current pt.
	    # 'ahp2p1' = Angle from Horizontal at P1 to line P1 -> P2, etc.

	    ahp2p1  = rptheta4 (xseg[1], yseg[1], lastp_x, lastp_y)
	    lastp_x = xseg[1]
	    lastp_y = yseg[1]
	    theta   = ahp2p1 - PIOVER6

	    # do even a single, interior point as a hexagon, up to lwidth times
	    do i = 1, lwidth {
		tx = xseg[1] + (2 + i) * DIAGSEP * cos (theta)
		ty = yseg[1] + (2 + i) * DIAGSEP * sin (theta)
		call plot (tx, ty, CCP_UP)

		# draw a bounding hexagon around point:
		do j = 1, 6 {
		    theta = theta + PIOVER3
		    tx = xseg[1] + (2 + i) * DIAGSEP * cos (theta)
		    ty = yseg[1] + (2 + i) * DIAGSEP * sin (theta)
		    call plot (tx, ty, CCP_DOWN)

		    # Store maximum-x plotted for a "newframe" in ccp_clear.
		    g_max_x = max (tx, g_max_x)
		}

		# fill in a diagonal line across hexagon:
		tx = xseg[1] + (2 + i) * DIAGSEP * cos (theta + PI)
		ty = yseg[1] + (2 + i) * DIAGSEP * sin (theta + PI)
		call plot (tx, ty, CCP_DOWN)
		theta = theta + PIOVER3				# rotate spokes
	    }

	} else {						# nsegpts > 1

	    if (g_lwover || lwidth == PL_SINGLE) {
		call plot (xseg[1], yseg[1], CCP_UP)
		g_max_x = max (xseg[1], g_max_x)

		do i = 2, nsegpts {
		    call plot (xseg[i], yseg[i], CCP_DOWN)
		    g_max_x = max (xseg[i], g_max_x)
		}
	    } else if (lwidth > PL_SINGLE) {

		# compute flanking points; by definition +-90 deg. from p1-p2,
		# so first point is special case; do for all thicknesses:

		call ccx_offsets (xseg[1]-xseg[2]+xseg[1], 
		    yseg[1]-yseg[2]+yseg[1], xseg[1],yseg[1], 
		    xseg[2],yseg[2], delx,dely)

		do i = 1, lwidth - 1 {
		    pleft_x[i]  = xseg[1] + i * delx
		    pleft_y[i]  = yseg[1] + i * dely
		    pright_x[i] = xseg[1] - i * delx
		    pright_y[i] = yseg[1] - i * dely
		}

		# must draw each segment individually, to make flanks meet.
	        do i = 1, nsegpts - 2 {

		    # actual line segment in data:
		    call plot (xseg[i], yseg[i], CCP_UP)
		    call plot (xseg[i+1], yseg[i+1], CCP_DOWN)
		    g_max_x = max (xseg[i], g_max_x)

		    call ccx_offsets (xseg[i],yseg[i], xseg[i+1],yseg[i+1],
			xseg[i+2],yseg[i+2], delx,dely)

		    # for each flanking line; p2 in middle, at temp origin
		    do j = 1, lwidth - 1 {

			# point to left of p1-p2, facing p2:
			dx = j * delx
			dy = j * dely
			tx = xseg[i+1] + dx
			ty = yseg[i+1] + dy
			call plot (pleft_x[j], pleft_y[j], CCP_UP)
			call plot (tx, ty, CCP_DOWN)
			pleft_x[j] = tx
			pleft_y[j] = ty

			# point to right of p1-p2, facing p2:
			tx = xseg[i+1] - dx
			ty = yseg[i+1] - dy
			call plot (pright_x[j], pright_y[j], CCP_UP)
			call plot (tx, ty, CCP_DOWN)
			pright_x[j] = tx
			pright_y[j] = ty
		    }
		}
		
		# last point:
		call plot (xseg[nsegpts-1], yseg[nsegpts-1], CCP_UP)
		call plot (xseg[nsegpts], yseg[nsegpts], CCP_DOWN)
		g_max_x = max (xseg[nsegpts-1], g_max_x)
		g_max_x = max (xseg[nsegpts], g_max_x)

		# save this point for a possible following dotted line segment:
		lastp_x = xseg[nsegpts]
		lastp_y = yseg[nsegpts]

		# square the flanking lines:
		call ccx_offsets (xseg[nsegpts-1], yseg[nsegpts-1],
		    xseg[nsegpts], yseg[nsegpts],
		    xseg[nsegpts] * 2.0 - xseg[nsegpts-1],
		    yseg[nsegpts] * 2.0 - yseg[nsegpts-1],
		    delx, dely)

		do i = 1, lwidth - 1 {
		    tx = xseg[nsegpts] + i * delx
		    ty = yseg[nsegpts] + i * dely
		    call plot (pleft_x[i], pleft_y[i], CCP_UP)
		    call plot (tx, ty, CCP_DOWN)
		    tx = xseg[nsegpts] - i * delx
		    ty = yseg[nsegpts] - i * dely
		    call plot (pright_x[i], pright_y[i], CCP_UP)
		    call plot (tx, ty, CCP_DOWN)
		}
	    }
	}
end


# CCX_OFFSETS -- return offsets in x, y from point 2 to one level of line width
# simulation, given points 1, 2, 3.

procedure ccx_offsets (p1x,p1y, p2x,p2y, p3x,p3y, delx,dely)

real	p1x,p1y		# input:  point 1 is previous point
real	p2x,p2y		# input:  point 2 is current point (middle of the three)
real	p3x,p3y		# input:  point 3 is succeeding point
real	delx,dely	# output: offsets from point 2 to one flanking point

real	ahp2p1		# Angle from Horizontal to line p2-->p1, etc.
real	ahp2p3, ap1p2p3, ahbisector, sintheta, r
real	rptheta4 ()
include	"ccp.com"

begin
	# convention is that p2 is current point, at temporary origin; p1
	# is "behind", and p3 is "ahead" of the current point, p2.
	# "ahp2p1"  = angle from horizontal to segment p2->p1
	# "ap1p2p3" = angle from p1 to p2 to p3
	# "ahbisector" = angle from horizontal (+x) to bisector of p1->p2->p3

	ahp2p1     = rptheta4 (p2x,p2y, p1x,p1y)
	ahp2p3	   = rptheta4 (p2x,p2y, p3x,p3y)
	ap1p2p3    = ahp2p1 - ahp2p3
	ahbisector = ahp2p3 + 0.5 * ap1p2p3
	sintheta   = sin (ahp2p1 - ahbisector)

	# very small angles cause extremely exaggerated vertices; truncate
	# at arbitrary multiple of plwsep; 10*plwsep is eqv. to 11.5 deg. bisect

	if (abs (sintheta) < SIN_MIN_HALFBISECTOR) {
	    r = g_plwsep / SIN_MIN_HALFBISECTOR
	    if (sintheta < 0.0)
		r = -r
	} else
	    r = g_plwsep / sintheta
	
	delx = r * cos (ahbisector)
	dely = r * sin (ahbisector)
end

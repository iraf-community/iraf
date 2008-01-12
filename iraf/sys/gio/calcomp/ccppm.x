# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <math.h>
include	"ccp.h"

define	DIAGSEP	(1.0 * g_plwsep / 0.7071068)	# dis at 40 degrees from plwsep

# CCP_POLYMARKER -- Draw a polymarker.  The polymarker is defined by the array
# of points P, consisting of successive (x,y) coordinate pairs.

procedure ccp_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pm
int	i, j, len_p
real	theta, x, y, tx, ty
include	"ccp.com"

begin
	if (npts <= 0)
	    return

	len_p = npts * 2

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Update polymarker attributes if necessary.

	pm = CCP_PMAP(g_cc)

	if (CCP_LTYPE(g_cc) != PM_LTYPE(pm)) {
	    call ccp_linetype (PM_LTYPE(pm))
	    CCP_LTYPE(g_cc) = PM_LTYPE(pm)
	}
	if (CCP_WIDTH(g_cc) != PM_WIDTH(pm)) 
		CCP_WIDTH(g_cc) = PM_WIDTH(pm)

	if (CCP_COLOR(g_cc) != PM_COLOR(pm)) {
	    call ccp_color (PM_COLOR(pm))
	    CCP_COLOR(g_cc) = PM_COLOR(pm)
	}

	# Draw the polymarker.
	do i = 1, len_p, 2 {
	    # Draw the single point as a box with a diagonal
	    # through it.

	    theta = 0.5 * HALFPI
	    x = XTRAN(p[i])
	    y = YTRAN(p[i+1])
	    tx = x + DIAGSEP * cos (theta)
	    ty = y + DIAGSEP * sin (theta)
	    call plot (tx, ty, CCP_UP)
	    g_max_x = max (tx, g_max_x)

	    do j = 1, 4 {
		theta = theta + HALFPI
		tx = x + DIAGSEP * cos (theta)
		ty = y + DIAGSEP * sin (theta)
		call plot (tx, ty, CCP_DOWN)
	    }

	    # Fill in diagonal.
	    tx = x + DIAGSEP * cos (theta + PI)
	    ty = y + DIAGSEP * sin (theta + PI)
	    call plot (tx, ty, CCP_DOWN)
	}
end

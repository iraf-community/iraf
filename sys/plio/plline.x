# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>
include <plset.h>
include <plio.h>

# PL_LINE -- Perform a rasterop operation upon a line of arbitrary width drawn
# at an arbitrary orientation in a 2-dimensional plane of a mask.  If the
# dimensionality of the mask exceeds 2, the pl_setplane() procedure should be
# called first to define the plane of the mask to be modified.

procedure pl_line (pl, x1, y1, x2, y2, width, rop)

pointer	pl			#I mask descriptor
int	x1,y1			#I start point of line
int	x2,y2			#I end point of line
int	width			#I width of line to be drawn, pixels
int	rop			#I rasterop defining operation

int	npts
int	x[4], y[4]
real	theta, hwidth, dx, dy

begin
	dx = x2 - x1
	dy = y2 - y1

	# Compute the line direction and halfwidth.
	hwidth = max (1.0, real(width)) / 2.0 - 0.001
	if (abs(dx) < 0.0001) {
	    if (dy > 0)
		theta = HALFPI
	    else
		theta = -HALFPI
	} else if (abs(dy) < 0.0001) {
	    if (dx > 0)
		theta = 0.0
	    else
		theta = PI
	} else
	    theta = atan2 (dy, dx)

	# Construct a polyline to be filled to draw the line.
	if (width < 1.0001) {
	    x[1] = x1;  y[1] = y1
	    x[2] = x2;  y[2] = y2
	    npts = 2

	} else {
	    x[1] = x1 + nint (hwidth * cos(theta+HALFPI))
	    y[1] = y1 + nint (hwidth * sin(theta+HALFPI))

	    x[2] = x1 + nint (hwidth * cos(theta-HALFPI))
	    y[2] = y1 + nint (hwidth * sin(theta-HALFPI))

	    x[3] = x2 + nint (hwidth * cos(theta-HALFPI))
	    y[3] = y2 + nint (hwidth * sin(theta-HALFPI))

	    x[4] = x2 + nint (hwidth * cos(theta+HALFPI))
	    y[4] = y2 + nint (hwidth * sin(theta+HALFPI))
	    npts = 4
	}

	# Draw the line.
	call pl_polygon (pl, x, y, npts, rop)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>

define	PIOVER4		(0.25 * PI)
define	THREEPIOVER4	(0.75 * TWOPI)

# RPTHETA4 -- Polar angle, Real precision, 4 arguments; from p1(x,y) to p2(x,y):
# angle between line segment p1-p2 and horizontal +x axis centered on p1; 
# returned in radians; single precision (see pdtheta4).

real procedure rptheta4 (p1x, p1y, p2x, p2y)

real	p1x,p1y, p2x,p2y	# x,y of each point
real	dx, dy, ang

begin
	dx = p2x - p1x
	dy = p2y - p1y

	if (dx == 0.0) {
	    if (dy >= 0.0) {
		ang = HALFPI
	    } else {
		ang = THREEPIOVER4
	    }
	} else {
	    ang = atan (dy / dx)
	    if (dx < 0.0) {		# 2nd or 3rd quadrant
		ang = ang + PI
	    } else if (dy < 0.0) {	# 4th quadrant
		ang = ang + TWOPI
	    }
	}

	return (ang)
end

include <mach.h>

define	QTOL	.125

# APQZERO - Solve for the root of a quadratic function defined by three
# points.

real procedure apqzero (x, y)

real	x[3]
real	y[3]

real	a, b, c, det, dx
real	x2, x3, y2, y3

begin
	# Compute the determinant.
	x2 = x[2] - x[1]
	x3 = x[3] - x[1]
	y2 = y[2] - y[1]
	y3 = y[3] - y[1]
	det = x2 * x3 * (x2 - x3)

	# Compute the shift in x.
	if (abs (det) > 100.0 * EPSILONR) {
	    a = (x3 * y2 - x2 * y3) / det
	    b = - (x3 * x3 * y2 - x2 * x2 * y3) / det
	    c =  a * y[1] / (b * b)
	    if (abs (c) > QTOL)
		dx = (-b / (2.0 * a)) * (1.0 - sqrt (1.0 - 4.0 * c))
	    else
		dx = - (y[1] / b) * (1.0 + c)
	    return (dx)
	} else if (abs (y3) > EPSILONR)
	    return (-y[1] * x3 / y3)
	else
	    return (0.0)
end

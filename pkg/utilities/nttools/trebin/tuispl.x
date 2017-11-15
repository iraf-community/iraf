# Interpolate at point X using cubic splines.  The array K must have
# previously been computed by calling TUCSPL.  Note that XA, YA, K
# are two-element subarrays of the arrays with the same names elsewhere.
# Input and output are all double precision.
#
# Copyright (c) 2017 Ole Streicher
#
# Reference: De Boor, Carl, et al. A practical guide to splines.
#            Vol. 27. New York: Springer-Verlag, 1978.

procedure tuispl (xa, ya, k, x, y)
double	  xa[2]	# pair of independent-variable values
double	  ya[2]	# pair of dependent-variable values
double	  k[2]	# derivatives of YA at each point
double	  x	# value at which spline is to be computed
double	  y	# interpolated value at X

double a, b, t
begin
    a = k[1] * (xa[2]-xa[1]) - (ya[2]-ya[1])
    b = -k[2] * (xa[2]-xa[1]) + (ya[2]-ya[1])
    t = (x - xa[1]) / (xa[2] - xa[1])
    y = (1 - t)*ya[1] + t*ya[2] + t*(1-t)*(a*(1-t) + b*t)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ELOGD -- Extended range log function.  Logarithmic scaling function for
# negative or partially negative data.  The function is piecewise, continuous,
# monotonic, reasonably smooth, and most importantly, is defined for all x.
#
#			     10.0 < X		y = log(x)
#	       -10.0 <= X <= 10.0		y = x / 10.0
#	   X < -10.0				y = -log(-x)
#
# Axes scaled with this function should have ticks labelled, e.g., 10**3,
# 10**2, 10**1, 0, -10**1, -10**2, -10**3.  The corresponding ticks for
# the normal log function would have values like 10**-2 rather than -10**2,
# hence it is not difficult to distinguish between the two functions.

double procedure elogd (x)

double	x

begin
	if (x > 10.0D0)
	    return (log10 (x))
	else if (x >= -10.0D0)
	    return (x / 10.0D0)
	else
	    return (-log10 (-x))
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AELOGR -- Inverse of the elogr function.

real procedure aelogr (x)

real	x

begin
	if (x > 1.0)
	    return (10.0 ** x)
	else if (x >= -1.0)
	    return (x * 10.0)
	else
	    return (-(10.0 ** (-x)))
end

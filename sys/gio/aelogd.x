# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AELOGD -- Inverse of the elogd function.

double procedure aelogd (x)

double	x

begin
	if (x > 1.0D0)
	    return (10.0D0 ** x)
	else if (x >= -1.0D0)
	    return (x * 10.0D0)
	else
	    return (- (10.0D0 ** (-x)))
end

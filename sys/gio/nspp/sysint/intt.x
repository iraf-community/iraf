# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<nspp.h>

# INTT -- Test whether the argument is an integer (return true) or a real
# (return false).  This works, hopefully, because legal NCAR metacode integers
# are always less than 2 ** 15, while real numbers will always appear to be
# large positive or negative integers.

bool procedure intt (value)

int	value

begin
	return (value > 0 && value < INTT_TESTVAL)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CPUTIME -- Return the difference between the current cpu time consumed
# and the argument, in long integer milliseconds.

long procedure cputime (old_cputime)

long	old_cputime, new_cputime
long	clk_time

begin
	call zgtime (clk_time, new_cputime)
	return (new_cputime - old_cputime)
end

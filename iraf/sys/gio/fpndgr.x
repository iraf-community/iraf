# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FP_NONDEGENR -- If two floating point numbers are equivalent to within the
# machine epsilon, adjust their values until a nondegenerate range is obtained.
# The boolean function returns true if it has to MAKE the range nondegenerate,
# i.e., if it modifies their values.

bool procedure fp_nondegenr (x1, x2)

real	x1, x2			# range to be adjusted
int	n
bool	fp_equalr()

begin
	for (n=0;  fp_equalr(x1,x2);  n=n+1) {
	    x1 = x1 - max (abs(x1) * 0.01, 0.01)
	    x2 = x2 + max (abs(x2) * 0.01, 0.01)
	}

	return (n > 0)
end

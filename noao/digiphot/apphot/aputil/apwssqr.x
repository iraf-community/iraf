# APWSSQR -- Procedure to compute the weighted sum of the squares.

real procedure apwssqr (a, wgt, npts)

real	a[ARB]		# data array
real	wgt[ARB]	# array of weights
int	npts		# number of points

int	i
real	sum

begin
	sum = 0.0
	do i = 1, npts
	    sum = sum + wgt[i] * a[i] * a[i]
	return (sum)
end

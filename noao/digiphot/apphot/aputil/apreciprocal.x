# APRECIPROCAL -- Procedure to take the reciprocal of the absolute value
# of a vector.

procedure apreciprocal (a, b, npts, value)

real	a[ARB]		# the input vector
real	b[ARB]		# the output vector
int	npts		# the number of data points
real	value		# the value to be assigned to b[i] if a[i] = 0

int	i

begin
	do i = 1, npts {
	    if (a[i] > 0.0)
		b[i] = 1.0 / a[i]
	    else if (a[i] < 0.0)
		b[i] = - 1.0 / a[i]
	    else
		b[i] = value
	}
end

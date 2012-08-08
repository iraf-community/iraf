# ARMSR -- Compute the rms of an array.

real procedure armsr (a, npoints)

real	a[ARB]				# Return rms of this array
int	npoints				# Number of points in the array

int	i
real	avg, rms

begin
	avg = 0.
	rms = 0.
	do i = 1, npoints {
	    avg = avg + a[i]
	    rms = rms + a[i] * a[i]
	}
	rms = sqrt ((npoints * rms - avg * avg) / (npoints * (npoints - 1)))

	return (rms)
end


# ARMSRR -- Compute the vector rms between two real arrays.

real procedure armsrr (a, b, npoints)

real	a[ARB]				# First array
real	b[ARB]				# Second array
int	npoints				# Number of points

int	i
real	residual, rms

begin
	rms = 0.
	do i = 1, npoints {
	    residual = a[i] - b[i]
	    rms = rms + residual ** 2
	}
	rms = sqrt (rms / npoints)

	return (rms)
end

# APMAPR -- Vector linear transformation.  Map the range of pixel values
# a1, a2 from a into the range b1, b2 into b.  It is assumed that a1 < a2
# and b1 < b2.

real procedure apmapr (a, a1, a2, b1, b2)

real	a		# the value to be mapped
real	a1, a2		# the numbers specifying the input data range
real	b1, b2		# the numbers specifying the output data range

real	minout, maxout, aoff, boff
real	scalar

begin
	scalar = (real (b2) - real (b1)) / (real (a2) - real (a1))
	minout = min (b1, b2)
	maxout = max (b1, b2)
	aoff = a1
	boff = b1
	return (max(minout, min(maxout, real((a - aoff) * scalar) + boff)))
end

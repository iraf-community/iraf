# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ADOT -- Vector inner or dot product.  The function value is the sum of the
# products of each pair of elements of the input vectors.

real procedure adots (a, b, npix)

short	a[ARB], b[ARB]

real	sum

int	npix, i

begin
	sum = 0
	do i = 1, npix
	    sum = sum + a[i] * b[i]

	return (sum)
end

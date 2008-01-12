# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ADOT -- Vector inner or dot product.  The function value is the sum of the
# products of each pair of elements of the input vectors.

double procedure adotd (a, b, npix)

double	a[ARB], b[ARB]

double	sum

int	npix, i

begin
	sum = 0.0D0
	do i = 1, npix
	    sum = sum + a[i] * b[i]

	return (sum)
end

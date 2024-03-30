# ludcmp -- lower-upper decomposition
# This routine decomposes a matrix (in-place) into lower and upper
# triangular portions.
#
# This routine simply calls the LU decomposition routine provided by LAPACK.

procedure ludcmp (a, n, np, indx, d)

real	a[np,np]	# io: input a, output decomposed a
int	n		# i: logical size of a is n x n
int	np		# i: space allocated for a
int	indx[n]		# o: index to be used by xt_lubksb
real	d		# o: +1 or -1

int	istat

begin
	d = 1.0
	call sgetrf(n, n, a, np, indx, istat)
end

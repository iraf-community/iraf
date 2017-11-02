# ludcmd -- lower-upper decomposition
# Double-precision version of ludcmp
# This routine decomposes a matrix (in-place) into lower and upper
# triangular portions.  Use lubksd to obtain a solution to A * X = B
# or to compute the inverse of the matrix A.
# If the matrix is singular, ISTAT is set to one.
#
# This routine simply calls the LU decomposition routine provided by LAPACK.

procedure ludcmd (a, n, np, indx, d, istat)

double	a[np,np]	# io: input a, output decomposed a
int	n		# i: logical size of a is n x n
int	np		# i: space allocated for a
int	indx[n]		# o: index to be used by xt_lubksb
double	d		# o: +1 or -1
int	istat		# o: OK if no problem; 1 if matrix is singular

begin
	d = 1.0
	call dgetrf(n, n, a, np, indx, istat)
	if (istat > 0)
	   istat = 1
end

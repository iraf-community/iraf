define	TINY	1.d-20

# ludcmd -- lower-upper decomposition
# Double-precision version of ludcmp from Numerical Recipes.
# This differs from the Numerical Recipes version in the following ways:
# (1) the calling sequence also includes an ISTAT parameter, (2) memory
# is allocated instead of using the fixed array VV, and (3) double
# precision is used.
# This routine decomposes a matrix (in-place) into lower and upper
# triangular portions.  Use lubksd to obtain a solution to A * X = B
# or to compute the inverse of the matrix A.
# If the matrix is singular, ISTAT is set to one.
#
# Phil Hodge, 28-Oct-1988  Subroutine copied from Numerical Recipes.
# Phil Hodge, 10-Sep-1992  Convert to double precision and rename from ludcmp.

procedure ludcmd (a, n, np, indx, d, istat)

double	a[np,np]	# io: input a, output decomposed a
int	n		# i: logical size of a is n x n
int	np		# i: space allocated for a
int	indx[n]		# o: index to be used by xt_lubksb
double	d		# o: +1 or -1
int	istat		# o: OK if no problem; 1 if matrix is singular
#--
pointer sp
pointer vv		# scratch space
double	aamax
double	sum
double	dum
int	i, j, k
int	imax

begin
	istat = OK				# initial value

	call smark (sp)
	call salloc (vv, n, TY_DOUBLE)

	d = 1.d0
	do i = 1, n {
	    aamax = 0.d0
	    do j = 1, n
		if (abs(a[i,j]) > aamax)
		    aamax = abs(a[i,j])
	    if (aamax == 0.d0) {
		istat = 1
		return
	    }
	    Memd[vv+i-1] = 1.d0 / aamax
	}
	do j = 1, n {
	    if (j > 1) {
		do i = 1, j-1 {
		    sum = a[i,j]
		    if (i > 1) {
			do k = 1, i-1
			    sum = sum - a[i,k] * a[k,j]
			a[i,j] = sum
		    }
		}
	    }
	    aamax = 0.d0
	    do i = j, n {
		sum = a[i,j]
		if (j > 1) {
		    do k = 1, j-1
			sum = sum - a[i,k] * a[k,j]
		    a[i,j] = sum
		}
		dum = Memd[vv+i-1] * abs[sum]
		if (dum >= aamax) {
		    imax = i
		    aamax = dum
		}
	    }
	    if (j != imax) {
		do k = 1, n {
		    dum = a[imax,k]
		    a[imax,k] = a[j,k]
		    a[j,k] = dum
		}
		d = -d
		Memd[vv+imax-1] = Memd[vv+j-1]
	    }
	    indx[j] = imax
	    if (j != n) {
		if (a[j,j] == 0.d0)
		    a[j,j] = TINY
		dum = 1.d0 / a[j,j]
		do i = j+1, n
		    a[i,j] = a[i,j] * dum
	    }
	}
	if (a[n,n] == 0.d0)
	    a[n,n] = TINY

	call sfree (sp)
end

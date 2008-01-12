define	TINY	1.e-20

# ludcmp -- lower-upper decomposition
# This routine decomposes a matrix (in-place) into lower and upper
# triangular portions.  This is the same as the Numerical Recipes version
# except that memory is allocated instead of using the fixed array VV.
#
# Phil Hodge, 28-Oct-1988  Subroutine copied from Numerical Recipes.

procedure ludcmp (a, n, np, indx, d)

real	a[np,np]	# io: input a, output decomposed a
int	n		# i: logical size of a is n x n
int	np		# i: space allocated for a
int	indx[n]		# o: index to be used by xt_lubksb
real	d		# o: +1 or -1
#--
pointer sp
pointer vv		# scratch space
real	aamax
real	sum
real	dum
int	i, j, k
int	imax

begin
	call smark (sp)
	call salloc (vv, n, TY_REAL)

	d = 1.
	do i = 1, n {
	    aamax = 0.
	    do j = 1, n
		if (abs(a[i,j]) > aamax)
		    aamax = abs(a[i,j])
	    if (aamax == 0.)
		call error (0, "singular matrix")
	    Memr[vv+i-1] = 1. / aamax
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
	    aamax = 0.
	    do i = j, n {
		sum = a[i,j]
		if (j > 1) {
		    do k = 1, j-1
			sum = sum - a[i,k] * a[k,j]
		    a[i,j] = sum
		}
		dum = Memr[vv+i-1] * abs[sum]
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
		Memr[vv+imax-1] = Memr[vv+j-1]
	    }
	    indx[j] = imax
	    if (j != n) {
		if (a[j,j] == 0.)
		    a[j,j] = TINY
		dum = 1. / a[j,j]
		do i = j+1, n
		    a[i,j] = a[i,j] * dum
	    }
	}
	if (a[n,n] == 0.)
	    a[n,n] = TINY

	call sfree (sp)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MULU -- Matrix utilities for MWCS.
#
#	mw_ludecompose		performs LU decomposition of a square matrix
#	mw_lubacksub		performs backsubstitution to solve a system
#
# These routines are derived from routines in the book Numerical Recipes,
# Press et. al. 1986.


# MW_LUDECOMPOSE -- Replace an NxN matrix A by the LU decomposition of a
# rowwise permutation of the matrix.  The LU decomposed matrix A and the
# permutation index IX are output.  The decomposition is performed in place.

procedure mw_ludecompose (a, ix, ndim)

double	a[ndim,ndim]		#U matrix to be inverted; inverted matrix
int	ix[ndim]		#O vector describing row permutation
int	ndim			#I dimension of square matrix

pointer	sp, vv
int	d, i, j, k, imax
double	aamax, sum, dum

begin
	call smark (sp)
	call salloc (vv, ndim, TY_DOUBLE)

	# Keep track of the number of row interchanges, odd or even (not used).
	d = 1

	# Loop over rows to get implicit scaling information.
	do i = 1, ndim {
	    aamax = 0.0
	    do j = 1, ndim
		if (abs(a[i,j]) > aamax)
		    aamax = abs(a[i,j])
	    if (aamax == 0.0)
		call error (1, "singular matrix")
	    Memd[vv+i-1] = 1.0 / aamax
	}

	# Loop over columns using Crout's method.
	do j = 1, ndim {
	    do i = 1, j-1 {
		sum = a[i,j]
		do k = 1, i-1
		    sum = sum - a[i,k] * a[k,j]
		a[i,j] = sum
	    }

	    # Search for the largest pivot element.
	    aamax = 0.0
	    do i = j, ndim {
		sum = a[i,j]
		do k = 1, j-1
		    sum = sum - a[i,k] * a[k,j]
		a[i,j] = sum

		# Figure of merit for the pivot.
		dum = Memd[vv+i-1] * abs(sum)
		if (dum >= aamax) {
		    imax = i
		    aamax = dum
		}
	    }

	    # Do we need to interchange rows?
	    if (j != imax) {
		# Yes, do so...
		do k = 1, ndim {
		    dum = a[imax,k]
		    a[imax,k] = a[j,k]
		    a[j,k] = dum
		}
		d = -d
		Memd[vv+imax-1] = Memd[vv+j-1]
	    }

	    ix[j] = imax
	    if (a[j,j] == 0.0)
		a[j,j] = EPSILOND

	    # Divide by the pivot element.
	    if (j != ndim) {
		dum = 1.0 / a[j,j]
		do i = j+1, ndim
		    a[i,j] = a[i,j] * dum
	    }
	}

	call sfree (sp)
end


# MW_LUBACKSUB -- Solves the set of N linear equations A*X=B.  Here A is input,
# not as the matrix A but rather as its LU decomposition, determined by the
# routine mw_ludecompose.  IX is input as the permutation vector as returned by
# mw_ludecompose.  B is input as the right hand side vector B, and returns with
# the solution vector X.

procedure mw_lubacksub (a, ix, b, ndim)

double	a[ndim,ndim]		#I LU decomposition of the matrix A
int	ix[ndim]		#I permutation vector for A
double	b[ndim]			#U rhs vector; solution vector
int	ndim			#I dimension of system

int	ii, ll, i, j
double	sum

begin
	# Do the forward substitution, unscrambling the permutation as we
	# go.  When II is set to a positive value, it will become the index
	# of the first nonvanishing element of B.

	ii = 0
	do i = 1, ndim {
	    ll = ix[i]
	    sum = b[ll]
	    b[ll] = b[i]

	    if (ii != 0) {
		do j = ii, i-1
		    sum = sum - a[i,j] * b[j]
	    } else if (sum != 0)
		ii = i

	    b[i] = sum
	}

	# Now do the backsubstitution.
	do i = ndim, 1, -1 {
	    sum = b[i]
	    if (i < ndim)
		do j = i+1, ndim
		    sum = sum - a[i,j] * b[j]
	    b[i] = sum / a[i,i]
	}
end

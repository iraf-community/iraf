# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MW_INVERTD -- Invert a square matrix, double precision version.  The matrix
# need not be symmetric.  The input and output matrices cannot be the same.

procedure mw_invertd (o_ltm, n_ltm, ndim)

double	o_ltm[ndim,ndim]	#I input matrix
double	n_ltm[ndim,ndim]	#O output (inverted) matrix
int	ndim			#I dimensionality of system

pointer	sp, ix, ltm
int	nelem, i, j

begin
	call smark (sp)

	nelem = ndim * ndim
	call salloc (ix, ndim, TY_INT)
	call salloc (ltm, nelem, TY_DOUBLE)

	# Make scratch copy (to be modified) of input matrix.
	call amovd (o_ltm, Memd[ltm], nelem)

	# Set up identity matrix.
	do i = 1, ndim {
	    do j = 1, ndim
		n_ltm[i,j] = 0.0
	    n_ltm[i,i] = 1.0
	}

	# Perform the LU decomposition.
	call mw_ludecompose (Memd[ltm], Memi[ix], ndim)

	# Compute the inverse matrix by backsubstitution.
	do j = 1, ndim
	    call mw_lubacksub (Memd[ltm], Memi[ix], n_ltm[1,j], ndim)

	call sfree (sp)
end

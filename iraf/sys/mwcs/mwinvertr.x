# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MW_INVERTR -- Invert a square matrix, single precision version.  The matrix
# need not be symmetric.  The input and output matrices should not be the same.

procedure mw_invertr (o_ltm, n_ltm, ndim)

real	o_ltm[ndim,ndim]	#I input matrix
real	n_ltm[ndim,ndim]	#O output (inverted) matrix
int	ndim			#I dimensionality of system

int	nelem, i, j
pointer	sp, ix, ltm, inv

begin
	call smark (sp)

	nelem = ndim * ndim
	call salloc (ix, ndim, TY_INT)
	call salloc (ltm, nelem, TY_DOUBLE)
	call salloc (inv, nelem, TY_DOUBLE)

	# Make scratch copy (to be modified) of input matrix.
	call achtrd (o_ltm, Memd[ltm], nelem)

	# Set up identity matrix.
	call aclrd (Memd[inv], nelem)
	do i = 1, ndim
	    Memd[inv+(i-1)*ndim+i-1] = 1.0

	# Perform the LU decomposition.
	call mw_ludecompose (Memd[ltm], Memi[ix], ndim)

	# Compute the inverse matrix by backsubstitution.
	do j = 1, ndim
	    call mw_lubacksub (Memd[ltm], Memi[ix], Memd[inv+(j-1)*ndim], ndim)

	# Output the inverted matrix.
	call achtdr (Memd[inv], n_ltm, nelem)

	call sfree (sp)
end

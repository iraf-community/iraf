# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MULU -- Matrix utilities for MWCS.
#
#	mw_ludecompose		performs LU decomposition of a square matrix
#	mw_lubacksub		performs backsubstitution to solve a system
#
# These routines simply call the LU composition routines provided by LAPACK.

#############################################################################
#  This code was copied/derived from the 'iraf-community' repository to
#  address known licensing issues with Numerical Recipes code.
#
#  Source Repository:   https://github.com/iraf-community/iraf
#  Author:              Ole Streicher
#############################################################################



# MW_LUDECOMPOSE -- Replace an NxN matrix A by the LU decomposition of a
# rowwise permutation of the matrix.  The LU decomposed matrix A and the
# permutation index IX are output.  The decomposition is performed in place.

procedure mw_ludecompose (a, ix, ndim)

double	a[ndim,ndim]		#U matrix to be inverted; inverted matrix
int	ix[ndim]		#O vector describing row permutation
int	ndim			#I dimension of square matrix

double	d
int	status

begin
	call ludcmd (a, ndim, ndim, ix, d, status)
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

begin
	call lubksd (a, ndim, ndim, ix, b)
end

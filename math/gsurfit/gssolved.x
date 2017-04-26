# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSSOLVE -- Solve the matrix normal equations of the form ca = b for a,
# where c is a symmetric, positive semi-definite, banded matrix with
# GS_NXCOEFF(sf) * GS_NYCOEFF(sf) rows and a and b are GS_NXCOEFF(sf) *
# GS_NYCOEFF(sf)-vectors.
# Initially c is stored in the matrix MATRIX
# and b is stored in VECTOR.
# The Cholesky factorization of MATRIX is calculated and stored in CHOFAC.
# Finally the coefficients are calculated by forward and back substitution
# and stored in COEFF.
#
# This version has two options: fit all the coefficients or fix the
# the zeroth coefficient at a specified reference point.

procedure dgssolve (sf, ier)

pointer	sf 		# curve descriptor
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

int	i, ncoeff
pointer	sp, vector, matrix

double	dgseval()

begin
	if (IS_INDEFD(GS_XREF(sf)) || IS_INDEFD(GS_YREF(sf)) ||
	    IS_INDEFD(GS_ZREF(sf)))
	    ncoeff = GS_NCOEFF(sf)
	else
	    ncoeff = GS_NCOEFF(sf) - 1

	# test for number of degrees of freedom
	ier = OK
	i = GS_NPTS(sf) - ncoeff
	if (i < 0) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	if (ncoeff == GS_NCOEFF(sf)) {
	    vector = GS_VECTOR(sf)
	    matrix = GS_MATRIX(sf)
	} else {
	    # allocate working space for the reduced vector and matrix
	    call smark (sp)
	    call salloc (vector, ncoeff, TY_DOUBLE)
	    call salloc (matrix, ncoeff*ncoeff, TY_DOUBLE)

	    # eliminate the terms from the vector and matrix
	    call amovd (VECTOR(GS_VECTOR(sf)+1), Memd[vector], ncoeff)
	    do i = 0, ncoeff-1
		call amovd (MATRIX(GS_MATRIX(sf)+(i+1)*GS_NCOEFF(sf)),
		    Memd[matrix+i*ncoeff], ncoeff)
	}

	# solve for the coefficients.
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    # calculate the Cholesky factorization of the data matrix
	    call dgschofac (MATRIX(matrix), ncoeff, ncoeff,
	        CHOFAC(GS_CHOFAC(sf)), ier)

	    # solve for the coefficients by forward and back substitution
	    call dgschoslv (CHOFAC(GS_CHOFAC(sf)), ncoeff, ncoeff,
	        VECTOR(vector), COEFF(GS_COEFF(sf)+GS_NCOEFF(sf)-ncoeff))

	default:
	    call error (0, "GSSOLVE: Illegal surface type.")
	}

	if (ncoeff != GS_NCOEFF(sf)) {
	    COEFF(GS_COEFF(sf)) = GS_ZREF(sf) -
	        dgseval (sf, GS_XREF(sf), GS_YREF(sf))
	    call sfree (sp)
	}
end

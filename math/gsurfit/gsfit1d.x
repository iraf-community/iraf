# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSFIT1 -- Procedure to solve the normal equations for a surface.
#
# This version modifies the fitting matrix to remove the first
# term from the fitting.  For the polynomial functions this means
# constraining the constant term to be zero.  Note that the first
# coefficent is still returned but with a value of zero.

procedure dgsfit1 (sf, x, y, z, w, npts, wtflag, ier)

pointer	sf		# surface descriptor
double	x[npts]		# array of x values
double	y[npts]		# array of y values
double	z[npts]		# data array
double	w[npts]		# array of weights
int	npts		# number of data points
int	wtflag		# type of weighting
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

begin
	    call dgszero (sf)
	    call dgsacpts (sf, x, y, z, w, npts, wtflag)
	    call dgssolve1 (sf, ier)
end


# GSSOLVE1 -- Solve the matrix normal equations of the form ca = b for
# a, where c is a symmetric, positive semi-definite, banded matrix with
# GS_NXCOEFF(sf) * GS_NYCOEFF(sf) rows and a and b are GS_NXCOEFF(sf) *
# GS_NYCOEFF(sf)-vectors.  Initially c is stored in the matrix MATRIX and b
# is stored in VECTOR.  The Cholesky factorization of MATRIX is calculated
# and stored in CHOFAC.  Finally the coefficients are calculated by forward
# and back substitution and stored in COEFF.
#
# This version modifies the fitting matrix to remove the first
# term from the fitting.  For the polynomial functions this means
# constraining the constant term to be zero.  Note that the first
# coefficent is still returned but with a value of zero.

procedure dgssolve1 (sf, ier)

pointer	sf 		# curve descriptor
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

int	i, ncoeff, offset
pointer	sp, vector, matrix

begin

	# test for number of degrees of freedom
	offset = 1
	ncoeff = GS_NCOEFF(sf) - offset
	ier = OK
	i = GS_NPTS(sf) - ncoeff
	if (i < 0) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	# allocate working space for the reduced vector and matrix
	call smark (sp)
	call salloc (vector, ncoeff, TY_DOUBLE)
	call salloc (matrix, ncoeff*ncoeff, TY_DOUBLE)

	# eliminate the first term from the vector and matrix
	call amovd (VECTOR(GS_VECTOR(sf)+offset), Memd[vector], ncoeff)
	do i = 0, ncoeff-1
	    call amovd (MATRIX(GS_MATRIX(sf)+(i+offset)*GS_NCOEFF(sf)),
		Memd[matrix+i*ncoeff], ncoeff)

	# solve for the coefficients.
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    # calculate the Cholesky factorization of the data matrix
	    call dgschofac (Memd[matrix], ncoeff, ncoeff,
	        CHOFAC(GS_CHOFAC(sf)), ier)

	    # solve for the coefficients by forward and back substitution
	    COEFF(GS_COEFF(sf)) = 0.
	    call dgschoslv (CHOFAC(GS_CHOFAC(sf)), ncoeff, ncoeff,
	        Memd[vector], COEFF(GS_COEFF(sf)+offset))

	default:
	    call error (0, "GSSOLVE1: Illegal surface type.")
	}

	call sfree (sp)
end

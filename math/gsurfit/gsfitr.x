# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "gsurfitdef.h"

# GSFIT -- Procedure to solve the normal equations for a surface.
# The inner products of the basis functions are calculated and
# accumulated into the GS_NCOEFF(sf) ** 2 matrix MATRIX.
# The main diagonal of the matrix is stored in the first row of
# MATRIX followed by the remaining non-zero diagonals.
# The inner product
# of the basis functions and the data ordinates are stored in the
# GS_NCOEFF(sf)-vector VECTOR. The Cholesky factorization of MATRIX
# is calculated and stored in CHOFAC. Forward and back substitution
# is used to solve for the GS_NCOEFF(sf)-vector COEFF.

procedure gsfit (sf, x, y, z, w, npts, wtflag, ier)

pointer	sf		# surface descriptor
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	z[npts]		# data array
real	w[npts]		# array of weights
int	npts		# number of data points
int	wtflag		# type of weighting
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

begin
	    call gszero (sf)
	    call gsacpts (sf, x, y, z, w, npts, wtflag)
	    call gssolve (sf, ier)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISLSOLVE -- Procedure to  solve for the x coefficients of image line
# number lineno. The inner products of the x basis functions are assumed
# to be in the SF_XORDER(sf) by SF_NXCOEFF(sf) array XMATRIX,
# while the inner products of the basis functions and
# the data ordinated for line number lineno are assumed to be in the
# lineno-th row of the SF_NXCOEFF(sf) by SF_NLINES(sf) matrix XCOEFF.
# The Cholesky factorization of XMATRIX is calculated and placed
# in XMATRIX overwriting the original data. The x coefficients for
# line number lineno are calculated and placed in the lineno-th row
# of XCOEFF replacing the original data.

procedure islsolve (sf, lineno, ier)

pointer	sf 		# pointer to the surface descriptor structure
int	lineno		# line being fitted in x
int	ier		# ier = 0, everything OK
			# ier = 1, matrix is singular
			# ier = 2, no degree of freedom

pointer	xcptr

begin
	# return if there are insuffucient points to solve the matrix
	ier = OK
	if ((SF_NXPTS(sf) - SF_NXCOEFF(sf)) < 0 ) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	# calculate the Cholesky factorization of the x matrix and store
	# separately for possible use by SFLREFIT

	call sfchofac (XMATRIX(SF_XMATRIX(sf)), SF_XORDER(sf), SF_NXCOEFF(sf),
	    XMATRIX(SF_XMATRIX(sf)), ier)

	# solve for the x coefficients for line lineno assuming the
	# data are in row lineno of xcoeff, the solution is placed
	# on top of the data

	xcptr = SF_XCOEFF(sf) + (lineno - 1) * SF_NXCOEFF(sf)
	call sfchoslv (XMATRIX(SF_XMATRIX(sf)), SF_XORDER(sf), SF_NXCOEFF(sf),
	    XCOEFF(xcptr), XCOEFF(xcptr))
end

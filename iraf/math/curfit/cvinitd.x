# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>
include	<mach.h>

include "dcurfitdef.h"

# CVINIT --  Procedure to initialize the curve  descriptor.

procedure dcvinit (cv, curve_type, order, xmin, xmax)

pointer	cv		# curve descriptor
int	curve_type	# type of curve to be fitted
int	order		# order of curve to be fitted, or in the case of the
			# spline the number of polynomial pieces to be fit
double	xmin		# minimum value of x
double	xmax		# maximum value of x

errchk	malloc, calloc

begin
	# check for bad parameters.
	cv = NULL
	if (order < 1)
	    call error (0, "CVINIT: Illegal order.")

	if (xmax <= xmin)
	    call error (0, "CVINIT: xmax <= xmin.")

	# allocate space for the curve descriptor
	call calloc (cv, LEN_CVSTRUCT, TY_STRUCT)

	# specify the curve-type dependent parameters
	switch (curve_type) {
	case CHEBYSHEV, LEGENDRE:
	    CV_ORDER(cv) = order
	    CV_NCOEFF(cv) = order
	    CV_RANGE(cv) = 2. / (xmax - xmin)
	    CV_MAXMIN(cv) = - (xmax + xmin) / 2.
	case SPLINE3:
	    CV_ORDER(cv) = SPLINE3_ORDER
	    CV_NCOEFF(cv) = order + SPLINE3_ORDER - 1
	    CV_NPIECES(cv) = order - 1
	    CV_SPACING(cv) = order / (xmax - xmin)
	case SPLINE1:
	    CV_ORDER(cv) = SPLINE1_ORDER
	    CV_NCOEFF(cv) = order + SPLINE1_ORDER - 1
	    CV_NPIECES(cv) = order - 1
	    CV_SPACING(cv) = order / (xmax - xmin)
	case USERFNC:
	    CV_ORDER(cv) = order
	    CV_NCOEFF(cv) = order
	    # Prevent abort for non-linear userfnc, where these values
	    #  may be arbitrary arguments to pass to external.
	    if ( abs(xmax-xmin) > EPSILON ) {
		CV_RANGE(cv) = 2. / (xmax - xmin)
	    } else {
		CV_RANGE(cv) = 0.
	    }
	    CV_MAXMIN(cv) = - (xmax + xmin) / 2.
	default:
	    call error (0, "CVINIT: Unknown curve type.")
	}

	# set remaining parameters
	CV_TYPE(cv) = curve_type
	CV_XMIN(cv) = xmin
	CV_XMAX(cv) = xmax

	# allocate space for the matrix and vectors
	call calloc (CV_XBASIS(cv), CV_ORDER(cv), TY_DOUBLE)
	call calloc (CV_MATRIX(cv), CV_ORDER(cv)*CV_NCOEFF(cv), TY_DOUBLE)
	call calloc (CV_CHOFAC(cv), CV_ORDER(cv)*CV_NCOEFF(cv), TY_DOUBLE)
	call calloc (CV_VECTOR(cv), CV_NCOEFF(cv), TY_DOUBLE)
	call calloc (CV_COEFF(cv), CV_NCOEFF(cv), TY_DOUBLE)

	# initialize pointer to basis functions to null
	CV_BASIS(cv) = NULL
	CV_WY(cv) = NULL
	CV_LEFT(cv) = NULL

	# set null user function
	CV_USERFNC(cv) = NULL

	# set data points counter
	CV_NPTS(cv) = 0
end

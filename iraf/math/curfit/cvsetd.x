# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>

include "dcurfitdef.h"

# CVSET -- Procedure to store the fit parameters derived from outside
# the CURFIT package inside the curve descriptor structure for use
# by the CVEVAL and CVVECTOR proocedures. The curve_type is one of
# LEGENDRE, CHEBYSHEV or SPLINE3. For the polynomials the number of
# coefficients is equal to one plus the order of the polynomial. In the
# case of the cubic spline the number of coefficients equals three plus
# the number of polynomial pieces. The polynomials are normalized over
# from xmin to xmax.

procedure dcvset (cv, curve_type, xmin, xmax, coeff, ncoeff)

pointer	cv		# curve descriptor
int	curve_type	# the functional form of the curve
double	xmin		# the minimum x value
double	xmax		# the maximum x value
double	coeff[ncoeff]	# the coefficient array
int	ncoeff		# the number of coefficients

errchk	malloc

begin
	# allocate space for curve descriptor
	call malloc (cv, LEN_CVSTRUCT, TY_STRUCT)

	if (ncoeff < 1)
	    call error (0, "CVSET: Illegal number of coefficients.")

	if (xmin >= xmax)
	    call  error (0, "CVSET: xmax <= xmin.")

	# set curve_type dependent curve descriptor parameters
	switch (curve_type) {
	case CHEBYSHEV, LEGENDRE:
	    CV_ORDER(cv) = ncoeff
	    CV_NCOEFF(cv) = ncoeff
	    CV_RANGE(cv) = 2. / (xmax - xmin)
	    CV_MAXMIN(cv) = - (xmax + xmin) / 2.
	case SPLINE3:
	    CV_ORDER(cv) = SPLINE3_ORDER
	    CV_NCOEFF(cv) = ncoeff
	    CV_NPIECES(cv) = ncoeff - SPLINE3_ORDER
	    CV_SPACING(cv) = (CV_NPIECES(cv) + 1) / (xmax - xmin) 
	case SPLINE1:
	    CV_ORDER(cv) = SPLINE1_ORDER
	    CV_NCOEFF(cv) = ncoeff
	    CV_NPIECES(cv) = ncoeff - SPLINE1_ORDER
	    CV_SPACING(cv) = (CV_NPIECES(cv) + 1) / (xmax - xmin) 
	case USERFNC:
	    CV_ORDER(cv) = ncoeff
	    CV_NCOEFF(cv) = ncoeff
	    CV_RANGE(cv) = 2. / (xmax - xmin)
	    CV_MAXMIN(cv) = - (xmax + xmin) / 2.
	default:
	    call error (0, "CVSET: Unknown curve type.")
	}

	# set remaining curve parameters
	CV_TYPE(cv) = curve_type
	CV_XMIN(cv) = xmin
	CV_XMAX(cv) = xmax

	# allocate space for xbasis and coefficient arrays, set remaining
	# pointers to NULL

	call malloc (CV_XBASIS(cv), CV_ORDER(cv), TY_DOUBLE)
	call malloc (CV_COEFF(cv), CV_NCOEFF(cv), TY_DOUBLE)

	CV_MATRIX(cv) = NULL
	CV_CHOFAC(cv) = NULL
	CV_VECTOR(cv) = NULL
	CV_BASIS(cv) = NULL
	CV_WY(cv) = NULL
	CV_LEFT(cv) = NULL

	CV_USERFNC(cv) = NULL

	# restore coefficients
	call amovd (coeff, COEFF(CV_COEFF(cv)), CV_NCOEFF(cv))
end

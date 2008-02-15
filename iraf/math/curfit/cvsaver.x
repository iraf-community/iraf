# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>

include "curfitdef.h"

# CVSAVE -- Procedure to save the parameters of the fit for later
# use by cveval and cvvector. Only curve_type, order, xmin, xmax
# and the coefficients are saved. The parameters are saved in fit
# in the order curve_type, order, xmin, xmax, followed by the
# coefficients.

procedure cvsave (cv, fit)

pointer	cv		# curve descriptor
real	fit[ARB]	# PIXEL array containing curve parameters

begin
	# set common curve parameters
	CV_SAVETYPE(fit) = CV_TYPE(cv)
	CV_SAVEXMIN(fit) = CV_XMIN(cv)
	CV_SAVEXMAX(fit) = CV_XMAX(cv)
	if (CV_TYPE(cv) == USERFNC)
	    CV_SAVEFNC(fit) = CV_USERFNCR(cv)	# no type conversion

	# set curve-type dependent parmeters
	switch (CV_TYPE(cv)) {
	case LEGENDRE, CHEBYSHEV, USERFNC:
	    CV_SAVEORDER(fit) = CV_ORDER(cv)
	case SPLINE1, SPLINE3:
	    CV_SAVEORDER(fit) = CV_NPIECES(cv) + 1
	default:
	    call error (0, "CVSAVE: Unknown curve type.")
	}


	# set coefficients
	if (CV_TYPE(cv) == USERFNC)
	    call amovr (COEFF(CV_COEFF(cv)), fit[CV_SAVECOEFF+1],
	        CV_NCOEFF(cv)) 
	else
	    call amovr (COEFF(CV_COEFF(cv)), fit[CV_SAVECOEFF],
	        CV_NCOEFF(cv)) 
end

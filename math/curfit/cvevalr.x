# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>

include "curfitdef.h"

# CVEVAL -- Procedure to evaluate curve at a given x. The CV_NCOEFF(cv)
# coefficients are assumed to be in COEFF.

real procedure cveval (cv, x)

pointer	cv		# curve descriptor
real	x		# x value

int	left
pointer	cptr, xptr
real	yfit

real	adotr()

begin

	# calculate the non-zero basis functions
	switch (CV_TYPE(cv)) {
	case CHEBYSHEV:
	    left = 0
	    call rcv_b1cheb (x, CV_ORDER(cv), CV_MAXMIN(cv), CV_RANGE(cv),
			   XBASIS(CV_XBASIS(cv)))
	case LEGENDRE:
	    left = 0
	    call rcv_b1leg (x, CV_ORDER(cv), CV_MAXMIN(cv), CV_RANGE(cv),
			   XBASIS(CV_XBASIS(cv)))
	case SPLINE3:
	    call rcv_b1spline3 (x, CV_NPIECES(cv), -CV_XMIN(cv),
	    		      CV_SPACING(cv), XBASIS(CV_XBASIS(cv)), left)
	case SPLINE1:
	    call rcv_b1spline1 (x, CV_NPIECES(cv), -CV_XMIN(cv),
	    		      CV_SPACING(cv), XBASIS(CV_XBASIS(cv)), left)
	case USERFNC:
	    left = 0
	    call rcv_b1user (cv, x)
	}


	# accumulate the fitted value
	cptr = CV_COEFF(cv) + left
	xptr = CV_XBASIS(cv)
	yfit = adotr (XBASIS(xptr), COEFF(cptr), CV_ORDER(cv))

	return (yfit)
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>

include "dcurfitdef.h"

# CVREFIT -- Procedure to refit the data assuming that the x and w values have
# not changed. MATRIX and CHOFAC are assumed to remain unchanged from the
# previous fit. It is only necessary to accumulate a new VECTOR and 
# calculate the coefficients COEFF by forward and back substitution. On
# the first call to cvrefit the basis functions for all data points are
# calculated and stored in BASIS. Subsequent calls to cvrefit reference these
# functions. Intervening calls to cvfit or cvzero zero the basis functions.

procedure dcvrefit (cv, x, y, w, ier)

pointer	cv		# curve descriptor
double	x[ARB]		# x array
double	y[ARB]		# y array
double	w[ARB]		# weight array
int	ier		# error code

int	i, k
pointer	bzptr
pointer	vzptr, vindex


begin
	# zero the right side of the matrix equation
	call aclrd (VECTOR(CV_VECTOR(cv)), CV_NCOEFF(cv))
	vzptr = CV_VECTOR(cv) - 1

	# if first call to cvrefit then calculate and store the basis
	# functions
	if (CV_BASIS(cv) == NULL) {

	    # allocate space for the basis functions and array containing
	    # the index of the first non-zero basis function
	    call malloc (CV_BASIS(cv), CV_NPTS(cv)*CV_ORDER(cv), TY_DOUBLE)
	    call malloc (CV_WY(cv), CV_NPTS(cv), TY_DOUBLE)

	    # calculate the non-zero basis functions
	    switch (CV_TYPE(cv)) {
	    case LEGENDRE:
		call dcv_bleg (x, CV_NPTS(cv), CV_ORDER(cv), CV_MAXMIN(cv),
		    CV_RANGE(cv), BASIS(CV_BASIS(cv)))
	    case CHEBYSHEV:
		call dcv_bcheb (x, CV_NPTS(cv), CV_ORDER(cv), CV_MAXMIN(cv),
			    CV_RANGE(cv), BASIS(CV_BASIS(cv)))
	    case SPLINE3:
	        call malloc (CV_LEFT(cv), CV_NPTS(cv), TY_INT)
		call dcv_bspline3 (x, CV_NPTS(cv), CV_NPIECES(cv),
		    -CV_XMIN(cv), CV_SPACING(cv), BASIS(CV_BASIS(cv)),
		    LEFT(CV_LEFT(cv)))
		call aaddki (LEFT(CV_LEFT(cv)), vzptr, LEFT(CV_LEFT(cv)),
		    CV_NPTS(cv))
	    case SPLINE1:
	        call malloc (CV_LEFT(cv), CV_NPTS(cv), TY_INT)
		call dcv_bspline1 (x, CV_NPTS(cv), CV_NPIECES(cv),
		    -CV_XMIN(cv), CV_SPACING(cv), BASIS(CV_BASIS(cv)),
		    LEFT(CV_LEFT(cv)))
		call aaddki (LEFT(CV_LEFT(cv)), vzptr, LEFT(CV_LEFT(cv)),
		    CV_NPTS(cv))
	    case USERFNC:
		call dcv_buser (cv, x, CV_NPTS(cv))
	    }
	}


	# accumulate the new right side of the matrix equation
	call amuld (w, y, Memd[CV_WY(cv)], CV_NPTS(cv))
	bzptr = CV_BASIS(cv)

	switch (CV_TYPE(cv)) {

	case SPLINE1, SPLINE3:

	    do k = 1, CV_ORDER(cv) {
	        do i = 1, CV_NPTS(cv) {
		    vindex = LEFT(CV_LEFT(cv)+i-1) + k
	            VECTOR(vindex) = VECTOR(vindex) + Memd[CV_WY(cv)+i-1] *
		        BASIS(bzptr+i-1)
		}
	        bzptr = bzptr + CV_NPTS(cv)
	    }

	case LEGENDRE, CHEBYSHEV, USERFNC:

	    do k = 1, CV_ORDER(cv) {
		vindex = vzptr + k
	        do i = 1, CV_NPTS(cv)
	            VECTOR(vindex) = VECTOR(vindex) + Memd[CV_WY(cv)+i-1] *
			BASIS(bzptr+i-1)
	        bzptr = bzptr + CV_NPTS(cv)
	    }

	}

	# solve for the new coefficients using forward and back
	# substitution
	call dcvchoslv (CHOFAC(CV_CHOFAC(cv)), CV_ORDER(cv), CV_NCOEFF(cv),
		    VECTOR(CV_VECTOR(cv)), COEFF(CV_COEFF(cv)))
end

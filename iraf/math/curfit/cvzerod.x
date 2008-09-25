# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "dcurfitdef.h"

# CVZERO -- Procedure to zero the accumulators before doing
# a new fit in accumulate mode. The inner products of the basis functions
# are accumulated in the CV_ORDER(cv) by CV_NCOEFF(cv) array MATRIX, while
# the inner products of the basis functions and the data ordinates are
# accumulated in the CV_NCOEFF(cv)-vector VECTOR.

procedure dcvzero (cv)

pointer	cv	# pointer to curve descriptor

size_t	sz_val

errchk	mfree

begin
	# zero the accumulators
	CV_NPTS(cv) = 0
	sz_val = CV_ORDER(cv)*CV_NCOEFF(cv)
	call aclrd (MATRIX(CV_MATRIX(cv)), sz_val)
	sz_val = CV_NCOEFF(cv)
	call aclrd (VECTOR(CV_VECTOR(cv)), sz_val)

	# free the basis functions defined from previous calls to cvrefit
	if (CV_BASIS(cv) != NULL) {
	    call mfree (CV_BASIS(cv), TY_DOUBLE)
	    call mfree (CV_WY(cv), TY_DOUBLE)
	    CV_BASIS(cv) = NULL
	    CV_WY(cv) = NULL
	    if (CV_LEFT(cv) != NULL) {
		call mfree (CV_LEFT(cv), TY_SIZE_T)
		CV_LEFT(cv) = NULL
	    }
	}
end

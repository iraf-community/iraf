# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "curfitdef.h"

# CVFREE -- Procedure to free the curve descriptor

procedure cvfree (cv)

pointer	cv	# the curve descriptor

errchk	mfree

begin
	if (cv == NULL)
	    return

	if (CV_XBASIS(cv) != NULL)
	    call mfree (CV_XBASIS(cv), TY_REAL)
	if (CV_VECTOR(cv) != NULL)
	    call mfree (CV_VECTOR(cv), TY_REAL)
	if (CV_COEFF(cv) != NULL)
	    call mfree (CV_COEFF(cv), TY_REAL)

	if (CV_BASIS(cv) != NULL)
	    call mfree (CV_BASIS(cv), TY_REAL)
	if (CV_LEFT(cv) != NULL)
	    call mfree (CV_LEFT(cv), TY_INT)
	if (CV_WY(cv) != NULL)
	    call mfree (CV_WY(cv), TY_REAL)

	if (CV_MATRIX(cv) != NULL)
	    call mfree (CV_MATRIX(cv), TY_REAL)
	if (CV_CHOFAC(cv) != NULL)
	    call mfree (CV_CHOFAC(cv), TY_REAL)

	call mfree (cv, TY_STRUCT)
end

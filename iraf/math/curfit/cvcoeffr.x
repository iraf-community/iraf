# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "curfitdef.h"

# CVCOEFF -- Procedure to fetch the number and magnitude of the coefficients.

procedure cvcoeff (cv, coeff, ncoeff)

pointer	cv		# curve descriptor
real	coeff[ARB]	# the coefficients of the fit
int	ncoeff		# the number of coefficients

size_t	sz_val

begin
	ncoeff = CV_NCOEFF(cv)

	# fetch coefficients
	sz_val = ncoeff
	call amovr (COEFF(CV_COEFF(cv)), coeff, sz_val)
end

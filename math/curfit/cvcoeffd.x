# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "dcurfitdef.h"

# CVCOEFF -- Procedure to fetch the number and magnitude of the coefficients.

procedure dcvcoeff (cv, coeff, ncoeff)

pointer	cv		# curve descriptor
double	coeff[ARB]	# the coefficients of the fit
int	ncoeff		# the number of coefficients

begin
	ncoeff = CV_NCOEFF(cv)

	# fetch coefficients
	call amovd (COEFF(CV_COEFF(cv)), coeff, ncoeff)
end

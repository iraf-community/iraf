# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gsurfitdef.h"

# GSCOEFF -- Procedure to fetch the number and magnitude of the coefficients.
# If the GS_XTERMS(sf) = YES then the number of coefficients will be
# GS_NXCOEFF(sf) * GS_NYCOEFF(sf) otherwise the number of 
# of coefficients will be (GS_NXCOEFF(sf) + GS_NYCOEFF(sf) - 1), and
# the GS_NXCOEFF(sf) x coefficients will be output first followed by
# the (GS_NYCOEFF(sf) - 1) y coefficients.

procedure gscoeff (sf, coeff, ncoeff)

pointer	sf		# pointer to the surface fitting descriptor
real	coeff[ARB]	# the coefficients of the fit
int	ncoeff		# the number of coefficients

begin
	# calculate the number of coefficients
	ncoeff = GS_NCOEFF(sf)
	call amovr (COEFF(GS_COEFF(sf)), coeff, ncoeff)
end

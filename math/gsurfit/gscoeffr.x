# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gsurfitdef.h"

# GSCOEFF -- Procedure to fetch the number and magnitude of the coefficients.
# If the GS_XTERMS(sf) = GS_XBI (YES) then the number of coefficients will be
# (GS_NXCOEFF(sf) * GS_NYCOEFF(sf)); if GS_XTERMS is GS_XTRI then the number
# of coefficients will be (GS_NXCOEFF(sf) *  GS_NYCOEFF(sf) - order *
# (order - 1) / 2) where order is the minimum of the x and yorders;  if
# GS_XTERMS(sf) = GS_XNONE then the number of coefficients will be
# (GS_NXCOEFF(sf) + GS_NYCOEFF(sf) - 1).

procedure gscoeff (sf, coeff, ncoeff)

pointer	sf		# pointer to the surface fitting descriptor
real	coeff[ARB]	# the coefficients of the fit
int	ncoeff		# the number of coefficients

begin
	# calculate the number of coefficients
	ncoeff = GS_NCOEFF(sf)
	call amovr (COEFF(GS_COEFF(sf)), coeff, ncoeff)
end

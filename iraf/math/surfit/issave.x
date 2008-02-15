# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/surfit.h>
include	"surfitdef.h"

# ISSAVE -- Procedure to save the surface fit for later use by the
# evaluate routines. After a call to SIFSAVE the first six elements
# of fit contain the surface type, xorder (or number of polynomial pieces
# in x), yorder (or the number of polynomial pieces in y), xterms, ncols
# and nlines. The remaining spaces are filled by the SF_NYCOEFF(sf) *
# SF_NXCOEFF(sf) surface coefficients. The coefficient of B(i,x) * B(j,y)
# is located in element number 6 + (i - 1) * SF_NYCOEFF(sf) + j of the
# array fit where i <= SF_NXCOEFF(sf) and j <= SF_NYCOEFF(sf).

procedure issave (sf, fit)

pointer	sf		# pointer to the surface descriptor
real	fit[ARB]	# array for storing fit

begin
	# get the surface parameters

	# order is surface type dependent
	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:
	    SF_SAVEXORDER(fit) = SF_XORDER(sf)
	    SF_SAVEYORDER(fit) = SF_YORDER(sf)
	case SF_SPLINE3, SF_SPLINE1:
	    SF_SAVEXORDER(fit) = SF_NXPIECES(sf) + 1
	    SF_SAVEYORDER(fit) = SF_NYPIECES(sf) + 1
	default:
	    call error (0, "SIFSAVE: Unknown surface type.")
	}

	# save remaining parameters
	SF_SAVETYPE(fit) = SF_TYPE(sf)
	SF_SAVENLINES(fit) = SF_NLINES(sf)
	SF_SAVENCOLS(fit) = SF_NCOLS(sf)
	SF_SAVEXTERMS(fit) = SF_XTERMS(sf)

	# save the coefficients
	call amovr (COEFF(SF_COEFF(sf)), fit[SF_SAVECOEFF+1], SF_NXCOEFF(sf) *
	    SF_NYCOEFF(sf))
end

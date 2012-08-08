# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSZERO -- Procedure to zero the accumulators before doing
# a new fit in accumulate mode. The inner products of the basis functions
# are accumulated in the GS_NCOEFF(sf) ** 2
# array MATRIX, while
# the inner products of the basis functions and the data ordinates are
# accumulated in the  NCOEFF(sf)-vector VECTOR.

procedure dgszero (sf)

pointer	sf	# pointer to surface descriptor
errchk	mfree

begin
	# zero the accumulators
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    GS_NPTS(sf) = 0
	    call aclrd (VECTOR(GS_VECTOR(sf)), GS_NCOEFF(sf))
	    call aclrd (MATRIX(GS_MATRIX(sf)), GS_NCOEFF(sf) ** 2)

	    # free the basis functions defined from previous calls to sfrefit
	    if (GS_XBASIS(sf) != NULL)
	        call mfree (GS_XBASIS(sf), TY_DOUBLE)
	    GS_XBASIS(sf) = NULL
	    if (GS_YBASIS(sf) != NULL)
	        call mfree (GS_YBASIS(sf), TY_DOUBLE)
	    GS_YBASIS(sf) = NULL
	    if (GS_WZ(sf) != NULL)
	        call mfree (GS_WZ(sf), TY_DOUBLE)
	    GS_WZ(sf) = NULL
	default:
	    call error (0, "GSZERO: Unknown surface type.")
	}
end

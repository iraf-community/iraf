# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISZERO -- Procedure to zero the accumulators for line number lineno.

procedure iszero (sf)

pointer	sf	# pointer to the surface descriptor

begin
	SF_NXPTS(sf) = 0
	SF_NYPTS(sf) = 0

	call aclrr (XMATRIX(SF_XMATRIX(sf)),
		     SF_XORDER(sf) * SF_NXCOEFF(sf))
	call aclrr (YMATRIX(SF_YMATRIX(sf)),
		    SF_YORDER(sf) * SF_NYCOEFF(sf))
	call aclrr (XCOEFF(SF_XCOEFF(sf)), SF_NXCOEFF(sf) * SF_NLINES(sf))
	call aclrr (COEFF(SF_COEFF(sf)), SF_NXCOEFF(sf) * SF_NYCOEFF(sf))

	if (SF_WZ(sf) != NULL)
	    call mfree (SF_WZ(sf), MEM_TYPE)
	if (SF_TLEFT(sf) != NULL)
	    call mfree (SF_TLEFT(sf), TY_INT)
end

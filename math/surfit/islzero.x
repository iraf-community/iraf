# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISLZERO -- Procedure to zero the accumulators for line number lineno.

procedure islzero (sf, lineno)

pointer	sf	# pointer to the surface descriptor
int	lineno	# line number
pointer	xcptr

begin
	SF_NXPTS(sf) = 0

	call aclrr (XMATRIX(SF_XMATRIX(sf)),
		     SF_XORDER(sf) * SF_NXCOEFF(sf))
	xcptr = SF_XCOEFF(sf) + (lineno - 1) * SF_NXCOEFF(sf)
	call aclrr (XCOEFF(xcptr), SF_NXCOEFF(sf))

	if (SF_WZ(sf) != NULL)
	    call mfree (SF_WZ(sf), MEM_TYPE)
	if (SF_TLEFT(sf) != NULL)
	    call mfree (SF_TLEFT(sf), TY_INT)
end

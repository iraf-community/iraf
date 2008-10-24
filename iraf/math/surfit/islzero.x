# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISLZERO -- Procedure to zero the accumulators for line number lineno.

procedure islzero (sf, lineno)

pointer	sf	# pointer to the surface descriptor
long	lineno	# line number

size_t	sz_val
pointer	xcptr

begin
	SF_NXPTS(sf) = 0

	sz_val = SF_XORDER(sf) * SF_NXCOEFF(sf)
	call aclrr (XMATRIX(SF_XMATRIX(sf)), sz_val)
	xcptr = SF_XCOEFF(sf) + (lineno - 1) * SF_NXCOEFF(sf)
	sz_val = SF_NXCOEFF(sf)
	call aclrr (XCOEFF(xcptr), sz_val)

	if (SF_WZ(sf) != NULL)
	    call mfree (SF_WZ(sf), MEM_TYPE)
	if (SF_TLEFT(sf) != NULL)
	    call mfree (SF_TLEFT(sf), TY_POINTER)
end

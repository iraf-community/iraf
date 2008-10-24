# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISZERO -- Procedure to zero the accumulators for line number lineno.

procedure iszero (sf)

pointer	sf	# pointer to the surface descriptor

size_t	sz_val

begin
	SF_NXPTS(sf) = 0
	SF_NYPTS(sf) = 0

	sz_val = SF_XORDER(sf) * SF_NXCOEFF(sf)
	call aclrr (XMATRIX(SF_XMATRIX(sf)), sz_val)
	sz_val = SF_YORDER(sf) * SF_NYCOEFF(sf)
	call aclrr (YMATRIX(SF_YMATRIX(sf)), sz_val)
	sz_val = SF_NXCOEFF(sf) * SF_NLINES(sf)
	call aclrr (XCOEFF(SF_XCOEFF(sf)), sz_val)
	sz_val = SF_NXCOEFF(sf) * SF_NYCOEFF(sf)
	call aclrr (COEFF(SF_COEFF(sf)), sz_val)

	if (SF_WZ(sf) != NULL)
	    call mfree (SF_WZ(sf), MEM_TYPE)
	if (SF_TLEFT(sf) != NULL)
	    call mfree (SF_TLEFT(sf), TY_POINTER)
end

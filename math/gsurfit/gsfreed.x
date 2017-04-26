# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "dgsurfitdef.h"

# GSFREE -- Procedure to free the surface descriptor

procedure dgsfree (sf)

pointer	sf	# the surface descriptor
errchk	mfree

begin
	if (sf == NULL)
	    return

	if (GS_XBASIS(sf) != NULL)
	    call mfree (GS_XBASIS(sf), TY_DOUBLE)
	if (GS_YBASIS(sf) != NULL)
	    call mfree (GS_YBASIS(sf), TY_DOUBLE)
	if (GS_MATRIX(sf) != NULL)
	    call mfree (GS_MATRIX(sf), TY_DOUBLE)
	if (GS_CHOFAC(sf) != NULL)
	    call mfree (GS_CHOFAC(sf), TY_DOUBLE)
	if (GS_VECTOR(sf) != NULL)
	    call mfree (GS_VECTOR(sf), TY_DOUBLE)
	if (GS_COEFF(sf) != NULL)
	    call mfree (GS_COEFF(sf), TY_DOUBLE)
	if (GS_WZ(sf) != NULL)
	    call mfree (GS_WZ(sf), TY_DOUBLE)

	if (sf != NULL)
	    call mfree (sf, TY_STRUCT)
end

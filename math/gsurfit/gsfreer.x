# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gsurfitdef.h"

# GSFREE -- Procedure to free the surface descriptor

procedure gsfree (sf)

pointer	sf	# the surface descriptor
errchk	mfree

begin
	if (sf == NULL)
	    return

	if (GS_XBASIS(sf) != NULL)
	    call mfree (GS_XBASIS(sf), TY_REAL)
	if (GS_YBASIS(sf) != NULL)
	    call mfree (GS_YBASIS(sf), TY_REAL)
	if (GS_MATRIX(sf) != NULL)
	    call mfree (GS_MATRIX(sf), TY_REAL)
	if (GS_CHOFAC(sf) != NULL)
	    call mfree (GS_CHOFAC(sf), TY_REAL)
	if (GS_VECTOR(sf) != NULL)
	    call mfree (GS_VECTOR(sf), TY_REAL)
	if (GS_COEFF(sf) != NULL)
	    call mfree (GS_COEFF(sf), TY_REAL)
	if (GS_WZ(sf) != NULL)
	    call mfree (GS_WZ(sf), TY_REAL)

	if (sf != NULL)
	    call mfree (sf, TY_STRUCT)
end

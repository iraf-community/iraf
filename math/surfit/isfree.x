# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISFREE -- Procedure to free the surface descriptor

procedure isfree (sf)

pointer	sf		# pointer to the surface descriptor
errchk	mfree

begin
	# free arrays in memory

	# first the basis functions
	if (SF_XBASIS(sf) != NULL)
	    call mfree (SF_XBASIS(sf), MEM_TYPE)
	if (SF_XLEFT(sf) != NULL)
	    call mfree (SF_XLEFT(sf), TY_INT)
	if (SF_YBASIS(sf) != NULL)
	    call mfree (SF_YBASIS(sf), MEM_TYPE)
	if (SF_YLEFT(sf) != NULL)
	    call mfree (SF_YLEFT(sf), TY_INT)

	# next the x and y matrices
	if (SF_XMATRIX(sf) != NULL)
	    call mfree (SF_XMATRIX(sf), MEM_TYPE)
	if (SF_YMATRIX(sf) != NULL)
	    call mfree (SF_YMATRIX(sf), MEM_TYPE)

	# last the coefficient matrices
	if (SF_XCOEFF(sf) != NULL)
	    call mfree (SF_XCOEFF(sf), MEM_TYPE)
	if (SF_COEFF(sf) != NULL)
	    call mfree (SF_COEFF(sf), MEM_TYPE)

	if (SF_WZ(sf) != NULL)
	    call mfree (SF_WZ(sf), MEM_TYPE)
	if (SF_TLEFT(sf) != NULL)
	    call mfree (SF_TLEFT(sf), TY_INT)

	# free surface descriptor
	if (sf != NULL)
	    call mfree (sf, TY_STRUCT)
end

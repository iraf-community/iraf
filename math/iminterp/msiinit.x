# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include	<math/iminterp.h>

# MSIINIT -- Procedure to initialize the sewquential 2D image interpolation
# package.  MSIINIT checks that the interpolant is one of the permitted
# types and allocates space for the interpolant descriptor structure.
# MSIINIT returns the pointer to the interpolant descriptor structure.

procedure msiinit (msi, interp_type)

pointer	msi		# pointer to the interpolant descriptor structure
int	interp_type	# interpolant type

errchk	malloc

begin
	if (interp_type < 1 || interp_type > II_NTYPES2D) {
	    call error (0, "MSIINIT: Illegal interpolant.")
	    return
	} else {
	    call malloc (msi, LEN_MSISTRUCT, TY_STRUCT)
	    MSI_TYPE(msi) = interp_type
	    MSI_COEFF(msi) = NULL
	}
end

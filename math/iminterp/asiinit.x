# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIINIT -- initialize the array sequential interpolant structure

procedure asiinit (asi, interp_type)

pointer	asi		# interpolant descriptor
int	interp_type	# interpolant type

begin
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0,"ASIINIT: Illegal interpolant type.")
	else {
	    call malloc (asi, LEN_ASISTRUCT, TY_STRUCT)
	    ASI_TYPE(asi) = interp_type
	    ASI_COEFF(asi) = NULL
	}

end

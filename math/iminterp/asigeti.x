# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include <math/iminterp.h>

# ASIGETI -- Procedure to fetch an asi integer parameter

int procedure asigeti (asi, param)

pointer	asi		# interpolant descriptor
int	param		# parameter to be fetched

begin
	switch (param) {
	case II_ASITYPE:
	    return (ASI_TYPE(asi))
	case II_ASINSAVE:
	    return (ASI_NSINC(asi) * ASI_NINCR(asi) + ASI_NCOEFF(asi) +
	        ASI_SAVECOEFF)
	case II_ASINSINC:
	    return (ASI_NSINC(asi))
	default:
	    call error (0, "ASIGETI: Unknown ASI parameter.")
	}
end

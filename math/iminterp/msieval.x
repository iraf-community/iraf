# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIEVAL -- Procedure to evaluate the interpolant at a single point.
# The procedure assumes that 1 <= x <= nxpix and that 1 <= y <= nypix.
# Checking for out of bounds pixels is the responsibility of the calling
# program.

real procedure msieval (msi, x, y)

pointer	msi		# pointer to the interpolant descriptor
real	x		# x data value
real	y		# y data value

real	value

begin
	switch (MSI_TYPE(msi)) {

	case II_BINEAREST:
	    call ii_binearest (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
		    	       MSI_NXCOEFF(msi), x, y, value, 1)
	    return (value)

	case II_BILINEAR:
	    call ii_bilinear (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		      MSI_NXCOEFF(msi), x, y, value, 1)
	    return (value)

	case II_BIPOLY3:
	    call ii_bipoly3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		     MSI_NXCOEFF(msi), x, y, value, 1)
	    return (value)

	case II_BIPOLY5:
	    call ii_bipoly5 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		     MSI_NXCOEFF(msi), x, y, value, 1)
	    return (value)

	case II_BISPLINE3:
	    call ii_bispline3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		       MSI_NXCOEFF(msi), x, y, value, 1)
	    return (value)

	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIEVAL -- Procedure to evaluate the interpolant at a single point.
# The procedure assumes that 1 <= x <= nxpix and that 1 <= y <= nypix.
# Checking for out of bounds pixels is the responsibility of the calling
# program.

real procedure msieval (msi, x, y)

pointer	msi		# pointer to the interpolant descriptor
real	x[ARB]		# x data value
real	y[ARB]		# y data value

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

	case II_BISINC:
	    call ii_bisinc (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), MSI_NYCOEFF(msi), x, y, value, 1,
		MSI_NSINC(msi), DX, DY)
	    return (value)

	case II_BILSINC:
	    call ii_bilsinc (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), MSI_NYCOEFF(msi), x, y, value, 1,
		LTABLE(MSI_LTABLE(msi)), 2 * MSI_NSINC(msi) + 1,
		MSI_NXINCR(msi), MSI_NYINCR(msi), DX, DY)
	    return (value)

	case II_BIDRIZZLE:
	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0) 
	        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            MSI_NXCOEFF(msi), x, y, value, 1, MSI_BADVAL(msi))
	    #else if (MSI_XPIXFRAC(msi) <= 0.0 && MSI_YPIXFRAC(msi) <= 0.0) 
	        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            #MSI_NXCOEFF(msi), x, y, value, 1, MSI_BADVAL(msi))
	    else
	        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            MSI_NXCOEFF(msi), x, y, value, 1, MSI_XPIXFRAC(msi),
		    MSI_YPIXFRAC(msi), MSI_BADVAL(msi))
	    return (value)

	}
end

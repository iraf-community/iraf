# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIVECTOR -- Procedure to evaluate the interpolant at an array of arbitrarily
# spaced points. The routines assume that 1 <= x <= nxpix and 1 <= y <= nypix.
# Checking for out of bounds pixels is the responsibility of the calling
# program.

procedure msivector (msi, x, y, zfit, npts)

pointer	msi		# pointer to the interpolant descriptor structure
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated

begin
	switch (MSI_TYPE(msi)) {

	case II_BINEAREST:
	    call ii_binearest (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), x, y, zfit, npts)

	case II_BILINEAR:
	    call ii_bilinear (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), x, y, zfit, npts)

	case II_BIPOLY3:
	    call ii_bipoly3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), x, y, zfit, npts)

	case II_BIPOLY5:
	    call ii_bipoly5 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), x, y, zfit, npts)

	case II_BISPLINE3:
	    call ii_bispline3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), x, y, zfit, npts)

	case II_BISINC:
	    call ii_bisinc (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), MSI_NYCOEFF(msi), x, y, zfit, npts,
		MSI_NSINC(msi), DX, DY)

	case II_BILSINC:
	    call ii_bilsinc (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	        MSI_NXCOEFF(msi), MSI_NYCOEFF(msi), x, y, zfit, npts,
		LTABLE(MSI_LTABLE(msi)), 2 * MSI_NSINC(msi) + 1,
		MSI_NXINCR(msi), MSI_NYINCR(msi), DX, DY)

	case II_BIDRIZZLE:
	    if (MSI_XPIXFRAC(msi) >= 1.0 && MSI_YPIXFRAC(msi) >= 1.0)
	        call ii_bidriz1 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            MSI_NXCOEFF(msi), x, y, zfit, npts, MSI_BADVAL(msi))
	    #else if (MSI_XPIXFRAC(msi) <= 0.0 && MSI_YPIXFRAC(msi) <= 0.0)
	        #call ii_bidriz0 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            #MSI_NXCOEFF(msi), x, y, zfit, npts, MSI_BADVAL(msi))
	    else
	        call ii_bidriz (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	            MSI_NXCOEFF(msi), x, y, zfit, npts, MSI_XPIXFRAC(msi),
		    MSI_YPIXFRAC(msi), MSI_BADVAL(msi))
	}
end

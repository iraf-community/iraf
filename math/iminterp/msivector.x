# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIVECTOR -- Procedure to evaluate the interpolant at an array of arbitrarily
# spaced points. The routines assume that 1 <= x <= nxpix and 1 <= y <= nypix.
# Checking for out of bounds pixels is the responsibility of the calling
# program.

procedure msivector (msi, x, y, zfit, npts)

pointer	msi		# pointer to the interpolant descriptor structure
real	x[npts]		# array of x values
real	y[npts]		# array of y values
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
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIGRID -- Procedure to evaluate the interpolant on a rectangular
# grid. The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix.
# The x and y vectors must be ordered such that x[i] < x[i+1] and
# y[i] < y[i+1].

procedure msigrid (msi, x, y, zfit, nx, ny, len_zfit)

pointer	msi 			# pointer to interpolant descriptor structure
real	x[nx]			# array of x values
real	y[ny]			# array of y values
real	zfit[len_zfit,ARB]	# array of fitted values
int	nx			# number of x points
int	ny			# number of y points
int	len_zfit		# row length of zfit

errchk	ii_grnearest, ii_grlinear, ii_grpoly3, ii_grpoly5, ii_grspline3

begin

	switch (MSI_TYPE(msi)) {

	case II_BINEAREST:
	    call ii_grnearest (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		       MSI_NXCOEFF(msi), x, y, zfit, nx, ny, len_zfit)
	case II_BILINEAR:
	    call ii_grlinear (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		      MSI_NXCOEFF(msi), x, y, zfit, nx, ny, len_zfit)
	case II_BIPOLY3:
	    call ii_grpoly3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		     MSI_NXCOEFF(msi), x, y, zfit, nx, ny, len_zfit)
	case II_BIPOLY5:
	    call ii_grpoly5 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		     MSI_NXCOEFF(msi), x, y, zfit, nx, ny, len_zfit)
	case II_BISPLINE3:
	    call ii_grspline3 (COEFF(MSI_COEFF(msi)), MSI_FSTPNT(msi),
	    		       MSI_NXCOEFF(msi), x, y, zfit, nx, ny, len_zfit)
	}
end

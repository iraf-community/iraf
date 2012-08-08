include <math/gsurfit.h>
include "xregister.h"

# RG_XSCALE -- Compute the background offset and x and y slope.

procedure rg_xscale (xc, data, npts, nx, ny, offset, coeff)

pointer	xc		#I pointer to the cross-correlation function
real	data[ARB]	#I the input data
int	npts		#I the number of points
int	nx, ny		#I the dimensions of the original subraster
real	offset		#I the input offset
real	coeff[ARB]	#O the output coefficients

int	wborder
pointer	gs
real	loreject, hireject, zero
int	rg_xstati(), rg_znsum(), rg_znmedian(), rg_slope()
real	rg_xstatr()

begin
	loreject = rg_xstatr (xc, LOREJECT)
	hireject = rg_xstatr (xc, HIREJECT)
	wborder = rg_xstati (xc, BORDER)

	switch (rg_xstati (xc, BACKGRD)) {
	case XC_BNONE:
	    coeff[1] = offset
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_MEAN:
	    if (rg_znsum (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_MEDIAN:
	    if (rg_znmedian (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_SLOPE:
	    call gsinit (gs, GS_POLYNOMIAL, 2, 2, GS_XNONE, 1.0, real (nx), 1.0,
	        real (ny))
	    if (rg_slope (gs, data, npts, nx, ny, wborder, wborder, loreject,
	        hireject) == ERR) {
		coeff[1] = 0.0
		coeff[2] = 0.0
		coeff[3] = 0.0
	    } else {
	        call gssave (gs, coeff)
		coeff[1] = coeff[GS_SAVECOEFF+1]
		coeff[2] = coeff[GS_SAVECOEFF+2]
		coeff[3] = coeff[GS_SAVECOEFF+3]
	    }
	    call gsfree (gs)
	default:
	    coeff[1] = offset
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	}
end

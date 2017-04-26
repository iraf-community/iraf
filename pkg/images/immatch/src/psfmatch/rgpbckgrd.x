include <math.h>
include <math/gsurfit.h>
include "psfmatch.h"

# RG_PSCALE -- Compute the background offset and x and y slope.

procedure rg_pscale (pm, data, npts, nx, ny, pnx, pny, offset, coeff)

pointer	pm		#I pointer to the psfmatch structure
real	data[ARB]	#I the input data
int	npts		#I the number of points
int	nx, ny		#I the dimensions of the original subraster
int	pnx, pny	#I the dimensions of the data region
real	offset		#I the input offset
real	coeff[ARB]	#O the output coefficients

int	wxborder, wyborder
pointer	gs
real	loreject, hireject, zero
int	rg_pstati(), rg_znsum(), rg_znmedian(), rg_slope()
real	rg_pstatr()

begin
	loreject = rg_pstatr (pm, LOREJECT)
	hireject = rg_pstatr (pm, HIREJECT)

	switch (rg_pstati (pm, BACKGRD)) {
	case PM_BNONE:
	    coeff[1] = 0.0
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case PM_BNUMBER:
	    coeff[1] = offset
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case PM_BMEAN:
	    if (rg_znsum (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case PM_BMEDIAN:
	    if (rg_znmedian (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case PM_BSLOPE:
	    call gsinit (gs, GS_POLYNOMIAL, 2, 2, GS_XNONE, 1.0, real (nx), 1.0,
	        real (ny))
	    wxborder = (nx - pnx) / 2
	    wyborder = (ny - pny) / 2
	    if (rg_slope (gs, data, npts, nx, ny, wxborder, wyborder, loreject,
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
	    coeff[1] = 0.0
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	}
end

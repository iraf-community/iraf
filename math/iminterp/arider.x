# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ARIDER -- Return the derivatives of the interpolant. The sinc function
# width and precision limits are hardwired to the builtin constants NSINC
# and DX. The look-up table sinc function is aliased to the sinc function.
# The drizzle function pixel fraction is harwired to the builtin constant
# PIXFRAC. If PIXFRAC is 1.0 then the drizzle  results are identical to the
# linear interpolation results.

procedure arider (x, datain, npix, derivs, nder, interp_type)

real	x[ARB]		# need 1 <= x <= n
real	datain[ARB]	# data values
int	npix		# number of data values
real	derivs[ARB]	# derivatives out -- derivs[1] is function value
int	nder		# total number of values returned in derivs
int	interp_type	# type of interpolator

int	i, j, k, nterms, nd, nearx
real	pcoeff[MAX_NDERIVS], accum, deltax, temp, tmpx[2]

begin
	if (nder <= 0)
	    return

	# Zero out the derivatives array.
	do i = 1, nder
	    derivs[i] = 0.

	switch (interp_type) {

	case II_NEAREST:
	    derivs[1] = datain[int (x[1] + 0.5)]
	    return

	case II_LINEAR:
	    nearx = x[1]
	    if (nearx >= npix)
		temp = 2. * datain[nearx] - datain[nearx-1]
	    else
		temp = datain[nearx+1]
	    derivs[1] = (x[1] - nearx) * temp + (nearx + 1 - x[1]) *
	        datain[nearx]
	    if (nder >= 2)
		derivs[2] = temp - datain[nearx]
	    return

	case II_SINC, II_LSINC:
	    call ii_sincder (x, derivs, nder, datain, npix, NSINC, DX)
	    return

	case II_DRIZZLE:
	    call ii_driz1 (x, derivs[1], 1, datain, BADVAL)
	    if (nder > 1) {
		deltax = x[2] - x[1]
		if (deltax == 0.0)
		    derivs[2] = 0.0
		else {
		    tmpx[1] = x[1]
		    tmpx[2] = (x[1] + x[2]) / 2.0
	    	    call ii_driz1 (x, temp, 1, datain, BADVAL)
		    tmpx[1] = tmpx[2]
		    tmpx[2] = x[2]
	    	    call ii_driz1 (x, derivs[2], 1, datain, BADVAL)
		    derivs[2] = 2.0 * (derivs[2] - temp) / deltax
		}
	    }
	    return

	case II_POLY3:
	    call ia_pcpoly3 (x, datain, npix, pcoeff)
	    nterms = 4

	case II_POLY5:
	    call ia_pcpoly5 (x, datain, npix, pcoeff)
	    nterms = 6

	case II_SPLINE3:
	    call ia_pcspline3 (x, datain, npix, pcoeff)
	    nterms = 4

	}

	# Evaluate the polynomial derivatives.

	nearx = x[1]
	deltax = x[1] - nearx

	nd = nder
	if (nder > nterms)
	    nd = nterms

	do k = 1, nd {	

	    # Evaluate using nested multiplication
	    accum = pcoeff[nterms - k + 1]
	    do j = nterms - k, 1, -1
		accum = pcoeff[j] + deltax * accum
	    derivs[k] = accum

	    # Differentiate.
	    do j = 1, nterms - k
		pcoeff[j] = j * pcoeff[j + 1]
	}
end

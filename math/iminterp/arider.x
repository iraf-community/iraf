# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ARIDER -- procedure to return the derivatives

procedure arider (x, datain, npix, derivs, nder, interp_type)

real	x		# need 1 <= x <= n
real	datain[ARB]	# data values
int	npix		# number of data values
real	derivs[ARB]	# derivatives out -- derivs[1] is function value
int	nder		# total number of values returned in derivs
int	interp_type	# type of interpolator

int	i, j, k, nterms, nd, nearx
real	pcoeff[MAX_NDERIVS], accum, deltax, temp

begin
	if (nder <= 0)
	    return

	# zero out derivs array
	do i = 1, nder
	    derivs[i] = 0.

	switch (interp_type) {

	case II_NEAREST:
	    derivs[1] = datain[int (x + 0.5)]
	    return

	case II_LINEAR:
	    nearx = x
	    if (nearx >= npix)
		temp = 2. * datain[nearx] - datain[nearx-1]
	    else
		temp = datain[nearx+1]
	    derivs[1] = (x - nearx) * temp + (nearx + 1 - x) * datain[nearx]
	    if (nder >= 2)
		derivs[2] = temp - datain[nearx]
	    return

	# The other cases call subroutines to generate polynomial coeff.
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

	nearx = x
	deltax = x - nearx
	nd = nder
	if (nder > nterms)
	    nd = nterms

	do k = 1, nd {	

	    # evaluate using nested multiplication
	    accum = pcoeff[nterms - k + 1]
	    do j = nterms - k, 1, -1
		accum = pcoeff[j] + deltax * accum
	    derivs[k] = accum

	    # differentiate
	    do j = 1, nterms - k
		pcoeff[j] = j * pcoeff[j + 1]
	}
end

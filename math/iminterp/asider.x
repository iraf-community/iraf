# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIDER -- Calculate nder derivatives assuming that x lands in the region
# 1 <= x <= npts.

procedure asider (asi, x, der, nder)

pointer	asi		# interpolant descriptor
real	x[ARB]		# x value
real	der[ARB]	# derivatives, der[1] is value der[2] is f prime 
int	nder		# number items returned = 1 + number of derivatives

int	nearx, i, j, k, nterms, nd
pointer	c0ptr, n0
real	deltax, accum, tmpx[2], pcoeff[MAX_NDERIVS], diff[MAX_NDERIVS]

begin
	# Return zero for derivatives that are zero.
	do i = 1, nder
	    der[i] = 0.

	# Nterms is number of terms in case polynomial type.
	nterms = 0

	# (c0ptr + 1) is the pointer to the first data point in COEFF.
	c0ptr = ASI_COEFF(asi) - 1 + ASI_OFFSET(asi)

	switch (ASI_TYPE(asi))	{

	case II_NEAREST:
	    der[1] = COEFF(c0ptr + int(x[1] + 0.5))
	    return

	case II_LINEAR:
	    nearx = x[1]
	    der[1] = (x[1] - nearx) * COEFF(c0ptr + nearx + 1) +
		     (nearx + 1 - x[1]) * COEFF(c0ptr + nearx)
	    if (nder > 1)
		der[2] = COEFF(c0ptr + nearx + 1) - COEFF(c0ptr + nearx)
	    return

	case II_SINC, II_LSINC:
	    call ii_sincder (x[1], der, nder,
		COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)), ASI_NCOEFF(asi),
		ASI_NSINC(asi), DX)
	    return

	case II_DRIZZLE:
	    if (ASI_PIXFRAC(asi) >= 1.0)
	        call ii_driz1 (x, der[1], 1, COEFF(ASI_COEFF(asi) +
	            ASI_OFFSET(asi)), ASI_BADVAL(asi))
	    else
	        call ii_driz (x, der[1], 1, COEFF(ASI_COEFF(asi) +
	            ASI_OFFSET(asi)), ASI_PIXFRAC(asi), ASI_BADVAL(asi))
	    if (nder > 1) {
		deltax = x[2] - x[1]
		if (deltax == 0.0)
		    der[2] = 0.0
		else {
		    tmpx[1] = x[1]
		    tmpx[2] = (x[1] + x[2]) / 2.0
	    	    if (ASI_PIXFRAC(asi) >= 1.0)
	                call ii_driz1 (tmpx, accum, 1, COEFF(ASI_COEFF(asi) +
	                    ASI_OFFSET(asi)), ASI_BADVAL(asi))
		    else
	                call ii_driz (tmpx, accum, 1, COEFF(ASI_COEFF(asi) +
	                    ASI_OFFSET(asi)), ASI_PIXFRAC(asi), ASI_BADVAL(asi))
		    tmpx[1] = tmpx[2]
		    tmpx[2] = x[2]
	    	    if (ASI_PIXFRAC(asi) >= 1.0)
	                call ii_driz1 (tmpx, der[2], 1, COEFF(ASI_COEFF(asi) +
	                    ASI_OFFSET(asi)), ASI_BADVAL(asi))
		    else
	                call ii_driz (tmpx, der[2], 1, COEFF(ASI_COEFF(asi) +
	                    ASI_OFFSET(asi)), ASI_PIXFRAC(asi), ASI_BADVAL(asi))
		    der[2] = 2.0 * (der[2] - accum) / deltax
		}
	    }
	    return

	case II_POLY3:
	    nterms = 4

	case II_POLY5:
	    nterms = 6

	case II_SPLINE3:
	    nterms = 4

	default:
	    call error (0, "ASIDER: Unknown interpolant type")
	}

	# Routines falls through to this point if the interpolant is one of
	# the higher order polynomial types or a third order spline.

	nearx = x[1]
	n0 = c0ptr + nearx
	deltax = x[1] - nearx

	# Compute the number of derivatives needed.
	nd = nder
	if (nder > nterms)
	    nd = nterms

	# Generate the polynomial coefficients.

	if (ASI_TYPE(asi) == II_SPLINE3) {

	    pcoeff[1] = COEFF(n0-1) + 4. * COEFF(n0) + COEFF(n0+1)
	    pcoeff[2] = 3. * (COEFF(n0+1) - COEFF(n0-1))
	    pcoeff[3] = 3. * (COEFF(n0-1) - 2. * COEFF(n0) + COEFF(n0+1))
	    pcoeff[4] = -COEFF(n0-1) + 3. * COEFF(n0) - 3. * COEFF(n0+1) +
				    COEFF(n0+2)

	# Newton's form written in line to get polynomial from data
	} else {

	    # Load data.
	    do i = 1, nterms
		diff[i] = COEFF(n0 - nterms/2 + i)

	    # Generate difference table.
	    do k = 1, nterms - 1
		do i = 1, nterms - k
		    diff[i] = (diff[i+1] - diff[i]) / k

	    # Shift to generate polynomial coefficients.
	    do k = nterms, 2, -1
		do i = 2,k
		    diff[i] = diff[i] + diff[i-1] * (k - i - nterms/2)
	    do i = 1,nterms
		pcoeff[i] = diff[nterms + 1 - i]
	}

	# Compute the derivatives. As the loop progresses pcoeff contains
	# coefficients of higher and higher derivatives.

	do k = 1, nd {

	    # Evaluate using nested multiplication.
	    accum = pcoeff[nterms - k + 1]
	    do j = nterms - k, 1, -1
		accum = pcoeff[j] + deltax * accum
	    der[k] = accum

	    # Differentiate polynomial.
	    do j = 1, nterms - k
		pcoeff[j] = j * pcoeff[j + 1]
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIDER -- Procedure to calculate nder derivatives assuming that x lands
# in the region 1 <= x <= npts.

procedure asider (asi, x, der, nder)

pointer	asi		# interpolant descriptor
real	x		# x value
real	der[ARB]	# derivatives, der[1] is value der[2] is f prime 
int	nder		# number items returned = 1 + number of derivatives

int	nearx, i, j, k, nterms, nd
real	deltax, accum, pcoeff[MAX_NDERIVS], diff[MAX_NDERIVS]
pointer	c0ptr, n0

begin
	# return zero for derivatives that are zero
	do i = 1, nder
	    der[i] = 0.

	# nterms is number of terms in case polynomial type
	nterms = 0

	# (c0ptr + 1) pointer to the first data point/coefficient in array
	c0ptr = ASI_COEFF(asi) - 1 + ASI_OFFSET(asi)

	switch (ASI_TYPE(asi))	{

	case II_NEAREST:
	    der[1] = COEFF(c0ptr + int(x + 0.5))
	    return

	case II_LINEAR:
	    nearx = x
	    der[1] = (x - nearx) * COEFF(c0ptr + nearx + 1) +
		     (nearx + 1 - x) * COEFF(c0ptr + nearx)

	    # try to return exaccumt number requested
	    if (nder > 1)
		der[2] = COEFF(c0ptr + nearx + 1) - COEFF(c0ptr + nearx)
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

	# falls through to here if interpolant is one of
	# the higher order polynomial types or third order spline

	nearx = x
	n0 = c0ptr + nearx
	deltax = x - nearx

	# no. of derivatives needed
	nd = nder
	if (nder > nterms)
	    nd = nterms

	# generate polynomial coefficients

	# spline first
	if (ASI_TYPE(asi) == II_SPLINE3) {

	    pcoeff[1] = COEFF(n0-1) + 4. * COEFF(n0) + COEFF(n0+1)
	    pcoeff[2] = 3. * (COEFF(n0+1) - COEFF(n0-1))
	    pcoeff[3] = 3. * (COEFF(n0-1) - 2. * COEFF(n0) + COEFF(n0+1))
	    pcoeff[4] = -COEFF(n0-1) + 3. * COEFF(n0) - 3. * COEFF(n0+1) +
				    COEFF(n0+2)

	# Newton's form written in line to get polynomial from data
	} else {

	    # load data
	    do i = 1, nterms
		diff[i] = COEFF(n0 - nterms/2 + i)

	    # generate difference table
	    do k = 1, nterms - 1
		do i = 1, nterms - k
		    diff[i] = (diff[i+1] - diff[i]) / k

	    # shift to generate polynomial coefficients of (x - n0)
	    do k = nterms, 2, -1
		do i = 2,k
		    diff[i] = diff[i] + diff[i-1] * (k - i - nterms/2)

	    do i = 1,nterms
		pcoeff[i] = diff[nterms + 1 - i]
	}

	do k = 1, nd {

	# as loop progresses pcoeff contains coefficients of
	# higher and higher derivatives

	    accum = pcoeff[nterms - k + 1]

	    # evaluate using nested multiplication
	    do j = nterms - k, 1, -1
		accum = pcoeff[j] + deltax * accum

	    der[k] = accum

	    # differentiate polynomial
	    do j = 1, nterms - k
		pcoeff[j] = j * pcoeff[j + 1]
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIGRL -- Procedure to find the integral of the interpolant from a to
# b be assuming that both a and b land in the array.

real procedure asigrl (asi, a, b)

pointer	asi		# interpolant descriptor
real	a		# lower limit for integral
real	b		# upper limit for integral

int	neara, nearb, i, j, nterms, index
real	deltaxa, deltaxb, accum, xa, xb, pcoeff[MAX_NDERIVS]
pointer	c0ptr, n0ptr

begin
	xa = a
	xb = b

	# flip order and sign at end
	if (a > b) {
	    xa = b
	    xb = a
	}

	# one index pointer
	c0ptr = ASI_COEFF(asi) - 1 + ASI_OFFSET(asi)

	neara = xa
	nearb = xb

	# zero accumulator
	accum = 0.

	# set number of terms
	switch (ASI_TYPE(asi)) {
	case II_NEAREST:
	    nterms = 0
	case II_LINEAR:
	    nterms = 1
	case II_POLY3:
	    nterms = 4
	case II_POLY5:
	    nterms = 6
	case II_SPLINE3:
	    nterms = 4
	}

	# NEAREST_NEIGHBOR and LINEAR are handled differently because of
	# storage.  Also probably good for speed.

	# NEAREST_NEIGHBOR
	if (nterms == 0) {

	    # reset segment to center values
	    neara = xa + 0.5
	    nearb = xb + 0.5

	    # set up for first segment
	    deltaxa = xa - neara

	    # for clarity one segment case is handled separately

	    # only one segment involved
	    if (nearb == neara) {
		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		accum = accum + (deltaxb - deltaxa) * COEFF(n0ptr)

	    # more than one segment
	    } else {

		# first segment
		n0ptr = c0ptr + neara
		accum = accum + (0.5 - deltaxa) * COEFF(n0ptr)

		# middle segment
		do j = neara + 1, nearb - 1 {
		    n0ptr = c0ptr + j
		    accum = accum + COEFF(n0ptr)
		}

		# last segment
		n0ptr = c0ptr + nearb
		deltaxb = xb - nearb
		accum = accum + (deltaxb + 0.5) * COEFF(n0ptr)
	    }

	# LINEAR
	} else if (nterms == 1) {

	    # set up for first segment
	    deltaxa = xa - neara

	    # for clarity one segment case is handled separately

	    # only one segment involved
	    if (nearb == neara) {
		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		accum = accum + (deltaxb - deltaxa) * COEFF(n0ptr) +
		     0.5 * (COEFF(n0ptr+1) - COEFF(n0ptr)) *
		     (deltaxb * deltaxb - deltaxa * deltaxa)

	    # more than one segment
	    } else {

		# first segment
		n0ptr = c0ptr + neara
		accum = accum + (1. - deltaxa) * COEFF(n0ptr) +
		     0.5 * (COEFF(n0ptr+1) - COEFF(n0ptr)) *
		     (1. - deltaxa * deltaxa)

		# middle segment
		do j = neara + 1, nearb - 1 {
		    n0ptr = c0ptr + j
		    accum = accum + 0.5 * (COEFF(n0ptr+1) + COEFF(n0ptr))
		}

		# last segment
		n0ptr = c0ptr + nearb
		deltaxb = xb - nearb
		accum = accum + COEFF(n0ptr) * deltaxb + 0.5 *
			(COEFF(n0ptr+1) - COEFF(n0ptr)) * deltaxb * deltaxb
	    }

	# A higher order interpolant
	} else {

	    # set up for first segment
	    deltaxa = xa - neara

	    # for clarity one segment case is handled separately

	    # only one segment involved
	    if (nearb == neara) {

		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		index = ASI_OFFSET(asi) + neara
		call ii_getpcoeff (COEFF(ASI_COEFF(asi)), index, pcoeff,
				  ASI_TYPE(asi))
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] *
		    	    (deltaxb ** i - deltaxa ** i)

	    # more than one segment
	    } else {

		# first segment
		index = ASI_OFFSET(asi) + neara
		call ii_getpcoeff (COEFF(ASI_COEFF(asi)), index, pcoeff,
				  ASI_TYPE(asi))
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * (1. - deltaxa ** i)

		# middle segment
		do j = neara + 1, nearb - 1 {
		    index = ASI_OFFSET(asi) + j
		    call ii_getpcoeff (COEFF(ASI_COEFF(asi)),
		    		      index, pcoeff, ASI_TYPE(asi))
		    do i = 1, nterms
			accum = accum + (1./i) * pcoeff[i]
		}

		# last segment
		index = ASI_OFFSET(asi) + nearb
		deltaxb = xb - nearb
		call ii_getpcoeff (COEFF(ASI_COEFF(asi)), index, pcoeff,
				   ASI_TYPE(asi))
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * deltaxb ** i
	    }
	}

	if (a < b)
	    return (accum)
	else
	    return (-accum)
end

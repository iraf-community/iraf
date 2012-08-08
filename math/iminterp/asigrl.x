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
	# Flip order and sign at end.
	xa = a
	xb = b
	if (a > b) {
	    xa = b
	    xb = a
	}

	# Initialize.
	c0ptr = ASI_COEFF(asi) - 1 + ASI_OFFSET(asi)
	neara = xa
	nearb = xb
	accum = 0.

	switch (ASI_TYPE(asi)) {
	case II_NEAREST, II_SINC, II_LSINC, II_DRIZZLE:
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

	# NEAREST_NEIGHBOR, LINEAR, SINC and LSINC are handled differently
	# because of storage.  Also probably good for speed in the case of
	# LINEAR and NEAREST_NEIGHBOUR.

	# NEAREST_NEIGHBOR
	switch (ASI_TYPE(asi)) {
	case II_NEAREST:

	    # Reset segment to center values.
	    neara = xa + 0.5
	    nearb = xb + 0.5

	    # Set up for first segment.
	    deltaxa = xa - neara

	    # For clarity one segment case is handled separately.

	    # Only one segment involved.
	    if (nearb == neara) {
		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		accum = accum + (deltaxb - deltaxa) * COEFF(n0ptr)

	    # More than one segment.
	    } else {

		# First segment.
		n0ptr = c0ptr + neara
		accum = accum + (0.5 - deltaxa) * COEFF(n0ptr)

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    n0ptr = c0ptr + j
		    accum = accum + COEFF(n0ptr)
		}

		# Last segment.
		n0ptr = c0ptr + nearb
		deltaxb = xb - nearb
		accum = accum + (deltaxb + 0.5) * COEFF(n0ptr)
	    }

	# LINEAR
	case II_LINEAR:

	    # Set up for first segment.
	    deltaxa = xa - neara

	    # For clarity one segment case is handled separately.

	    # Only one segment is involved.
	    if (nearb == neara) {
		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		accum = accum + (deltaxb - deltaxa) * COEFF(n0ptr) +
		     0.5 * (COEFF(n0ptr+1) - COEFF(n0ptr)) *
		     (deltaxb * deltaxb - deltaxa * deltaxa)

	    # More than one segment.
	    } else {

		# First segment.
		n0ptr = c0ptr + neara
		accum = accum + (1. - deltaxa) * COEFF(n0ptr) +
		     0.5 * (COEFF(n0ptr+1) - COEFF(n0ptr)) *
		     (1. - deltaxa * deltaxa)

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    n0ptr = c0ptr + j
		    accum = accum + 0.5 * (COEFF(n0ptr+1) + COEFF(n0ptr))
		}

		# Last segment.
		n0ptr = c0ptr + nearb
		deltaxb = xb - nearb
		accum = accum + COEFF(n0ptr) * deltaxb + 0.5 *
			(COEFF(n0ptr+1) - COEFF(n0ptr)) * deltaxb * deltaxb
	    }

	# SINC
	case II_SINC, II_LSINC:
	    call ii_sincigrl (xa, xb, accum, COEFF(ASI_COEFF(asi) +
	        ASI_OFFSET(asi)), ASI_NCOEFF(asi), ASI_NSINC(asi), DX)

	# DRIZZLE
	case II_DRIZZLE:
	    if (ASI_PIXFRAC(asi) >= 1.0)
	        call ii_dzigrl1 (xa, xb, accum, COEFF(ASI_COEFF(asi) +
	            ASI_OFFSET(asi)))
	    else
	        call ii_dzigrl (xa, xb, accum, COEFF(ASI_COEFF(asi) +
	            ASI_OFFSET(asi)), ASI_PIXFRAC(asi))

	# A higher order interpolant.
	default:

	    # Set up for first segment.
	    deltaxa = xa - neara

	    # For clarity one segment case is handled separately.

	    # Only one segment involved.
	    if (nearb == neara) {

		deltaxb = xb - nearb
		n0ptr = c0ptr + neara
		index = ASI_OFFSET(asi) + neara
		call ii_getpcoeff (COEFF(ASI_COEFF(asi)), index, pcoeff,
				  ASI_TYPE(asi))
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] *
		    	    (deltaxb ** i - deltaxa ** i)

	    # More than one segment.
	    } else {

		# First segment.
		index = ASI_OFFSET(asi) + neara
		call ii_getpcoeff (COEFF(ASI_COEFF(asi)), index, pcoeff,
				  ASI_TYPE(asi))
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * (1. - deltaxa ** i)

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    index = ASI_OFFSET(asi) + j
		    call ii_getpcoeff (COEFF(ASI_COEFF(asi)),
		    		      index, pcoeff, ASI_TYPE(asi))
		    do i = 1, nterms
			accum = accum + (1./i) * pcoeff[i]
		}

		# Last segment.
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

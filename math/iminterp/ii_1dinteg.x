# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include <math/iminterp.h>

# II_1DINTEG -- Procedure to find the integral of the interpolant from a to
# b be assuming that both a and b land in the array.

real procedure ii_1dinteg (coeff, a, b, interp_type)

real	coeff[ARB]	# 1D array of coefficients
real	a		# lower limit for integral
real	b		# upper limit for integral
int	interp_type	# type of 1D interpolant

int	neara, nearb, i, j, nterms
real	deltaxa, deltaxb, accum, xa, xb, pcoeff[MAX_NDERIVS]

begin
	xa = a
	xb = b

	# flip order and sign at end
	if (a > b) {
	    xa = b
	    xb = a
	}

	neara = xa
	nearb = xb

	# zero accumulator
	accum = 0.

	# set number of terms
	switch (interp_type) {
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
		accum = accum + (deltaxb - deltaxa) * coeff[neara]

	    # more than one segment
	    } else {

		# first segment
		accum = accum + (0.5 - deltaxa) * coeff[neara]

		# middle segment
		do j = neara + 1, nearb - 1 {
		    accum = accum + coeff[j]
		}

		# last segment
		deltaxb = xb - nearb
		accum = accum + (deltaxb + 0.5) * coeff[nearb]
	    }

	# LINEAR
	} else if (nterms == 1) {

	    # set up for first segment
	    deltaxa = xa - neara

	    # for clarity one segment case is handled separately

	    # only one segment involved
	    if (nearb == neara) {

		deltaxb = xb - nearb
		accum = accum + (deltaxb - deltaxa) * coeff[neara] +
		     0.5 * (coeff[neara+1] - coeff[neara]) *
		     (deltaxb * deltaxb - deltaxa * deltaxa)

	    # more than one segment
	    } else {

		# first segment
		accum = accum + (1. - deltaxa) * coeff[neara] +
		     0.5 * (coeff[neara+1] - coeff[neara]) *
		     (1. - deltaxa * deltaxa)

		# middle segment
		do j = neara + 1, nearb - 1 {
		    accum = accum + 0.5 * (coeff[j+1] + coeff[j])
		}

		# last segment
		deltaxb = xb - nearb
		accum = accum + coeff[nearb] * deltaxb + 0.5 *
			(coeff[nearb+1] - coeff[nearb]) * deltaxb * deltaxb
	    }

	# A higher order interpolant
	} else {

	    # set up for first segment
	    deltaxa = xa - neara

	    # for clarity one segment case is handled separately

	    # only one segment involved
	    if (nearb == neara) {

		deltaxb = xb - nearb
		call ii_getpcoeff (coeff, neara, pcoeff, interp_type)
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] *
		    	    (deltaxb ** i - deltaxa ** i)

	    # more than one segment
	    } else {

		# first segment
		call ii_getpcoeff (coeff, neara, pcoeff, interp_type)
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * (1. - deltaxa ** i)

		# middle segment
		do j = neara + 1, nearb - 1 {
		    call ii_getpcoeff (coeff, j, pcoeff, interp_type)

		    do i = 1, nterms
			accum = accum + (1./i) * pcoeff[i]
		}

		# last segment
		deltaxb = xb - nearb
		call ii_getpcoeff (coeff, nearb, pcoeff, interp_type)
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * deltaxb ** i
	    }
	}

	if (a < b)
	    return (accum)
	else
	    return (-accum)
end

	
# II_GETPCOEFF -- Generates polynomial coefficients if SPLINE3, POLY3 or POLY5.

procedure ii_getpcoeff (coeff, index, pcoeff, interp_type)

real	coeff[ARB]	# coefficient array
int	index		# coefficients wanted for index < x < index + 1
real	pcoeff[ARB]	# polynomial coefficients
int	interp_type	# type of interpolant

int	i, k, nterms
real	diff[MAX_NDERIVS]

begin
	# generate polynomial coefficients, first for spline

	if (interp_type == II_SPLINE3) {

	    pcoeff[1] = coeff[index-1] + 4. * coeff[index] + coeff[index+1]
	    pcoeff[2] = 3. * (coeff[index+1] - coeff[index-1])
	    pcoeff[3] = 3. * (coeff[index-1] - 2. * coeff[index] +
	    		coeff[index+1])
	    pcoeff[4] = -coeff[index-1] + 3. * coeff[index] -
	    		3. * coeff[index+1] + coeff[index+2]
	} else {

	    if (interp_type == II_POLY5)
		nterms = 6

	    # must be POLY3
	    else
		nterms = 4

	    # Newton's form written in line to get polynomial from data

	    # load data
	    do i = 1, nterms
		diff[i] = coeff[index - nterms/2 + i]

	    # generate difference table
	    do k = 1, nterms - 1
		do i = 1, nterms - k
		    diff[i] = (diff[i+1] - diff[i]) / k

	    # shift to generate polynomial coefficients of (x - index)
	    do k = nterms, 2, -1
		do i = 2, k
		    diff[i] = diff[i] + diff[i-1] * (k - i - nterms/2)

	    do i = 1, nterms
		pcoeff[i] = diff[nterms + 1 - i]
	}
end

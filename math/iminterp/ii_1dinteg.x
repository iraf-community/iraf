# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include <math/iminterp.h>

# II_1DINTEG -- Find the integral of the interpolant from a to b be assuming
# that both a and b land in the array. This routine is not used directly
# in the 1D interpolation package but is actually called repeatedly from the
# 2D interpolation package. Therefore the SINC function interpolator has
# not been implemented, although it has been blocked in.

real procedure ii_1dinteg (coeff, len_coeff, a, b, interp_type, nsinc, dx,
	pixfrac)

real	coeff[ARB]	# 1D array of coefficients
int	len_coeff	# length of coefficient array (used in sinc only)
real	a		# lower limit for integral
real	b		# upper limit for integral
int	interp_type	# type of 1D interpolant
int	nsinc		# width of sinc function
real	dx		# sinc precision
real	pixfrac		# drizzle pixel fraction

int	neara, nearb, i, j, nterms
real	deltaxa, deltaxb, accum, xa, xb, pcoeff[MAX_NDERIVS]

begin
	# Flip order and sign at end.
	xa = a
	xb = b
	if (a > b) {
	    xa = b
	    xb = a
	}

	# Initialize.
	neara = xa
	nearb = xb
	accum = 0.

	switch (interp_type) {
	case II_NEAREST:
	    nterms = 0
	case II_LINEAR:
	    nterms = 1
	case II_DRIZZLE:
	    nterms = 0
	case II_POLY3:
	    nterms = 4
	case II_POLY5:
	    nterms = 6
	case II_SPLINE3:
	    nterms = 4
	case II_SINC, II_LSINC:
	    nterms = 0
	}

	switch (interp_type) {
	# NEAREST
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
		accum = accum + (deltaxb - deltaxa) * coeff[neara]

	    # More than one segment.
	    } else {

		# First segment.
		accum = accum + (0.5 - deltaxa) * coeff[neara]

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    accum = accum + coeff[j]
		}

		# Last segment.
		deltaxb = xb - nearb
		accum = accum + (deltaxb + 0.5) * coeff[nearb]
	    }

	# LINEAR
	case II_LINEAR:

	    # Set up for first segment.
	    deltaxa = xa - neara

	    # For clarity one segment case is handled separately.

	    # Only one segment involved.
	    if (nearb == neara) {

		deltaxb = xb - nearb
		accum = accum + (deltaxb - deltaxa) * coeff[neara] +
		     0.5 * (coeff[neara+1] - coeff[neara]) *
		     (deltaxb * deltaxb - deltaxa * deltaxa)

	    # More than one segment.
	    } else {

		# First segment.
		accum = accum + (1. - deltaxa) * coeff[neara] +
		     0.5 * (coeff[neara+1] - coeff[neara]) *
		     (1. - deltaxa * deltaxa)

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    accum = accum + 0.5 * (coeff[j+1] + coeff[j])
		}

		# Last segment.
		deltaxb = xb - nearb
		accum = accum + coeff[nearb] * deltaxb + 0.5 *
			(coeff[nearb+1] - coeff[nearb]) * deltaxb * deltaxb
	    }

	# DRIZZLE-- Note that to get get pixfrac an interface change was
	# required.
	case II_DRIZZLE:
	    if (pixfrac >= 1.0)
		call ii_dzigrl1 (a, b, accum, coeff)
	    else
		call ii_dzigrl (a, b, accum, coeff, pixfrac)

	# SINC -- Note that to get ncoeff, nsinc, and dx,  an interface change
	# was required. 
	case II_SINC, II_LSINC:
	    call ii_sincigrl (xa, xb, accum, coeff, len_coeff, nsinc, dx)

	# A higher order interpolant.
	default:

	    # Set up for first segment.
	    deltaxa = xa - neara

	    # For clarity one segment case is handled separately.

	    # Only one segment involved.
	    if (nearb == neara) {

		deltaxb = xb - nearb
		call ii_getpcoeff (coeff, neara, pcoeff, interp_type)
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] *
		    	    (deltaxb ** i - deltaxa ** i)

	    # More than one segment.
	    } else {

		# First segment.
		call ii_getpcoeff (coeff, neara, pcoeff, interp_type)
		do i = 1, nterms
		    accum = accum + (1./i) * pcoeff[i] * (1. - deltaxa ** i)

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    call ii_getpcoeff (coeff, j, pcoeff, interp_type)

		    do i = 1, nterms
			accum = accum + (1./i) * pcoeff[i]
		}

		# Last segment.
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

	
# II_GETPCOEFF -- Generates polynomial coefficients if the interpolant is
# SPLINE3, POLY3 or POLY5.

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


# II_SINCIGRL -- Evaluate integral of sinc interpolator.
# The integral is computed by dividing interval into a number of equal
# size subintervals which are at most one pixel wide.  The integral
# of each subinterval is the central value times the interval width.

procedure ii_sincigrl (a, b, sum, data, npix, nsinc, mindx)

real	a, b			# integral limits
real	sum			# output integral value
real	data[npix]		# input data array
int	npix			# number of pixels
int	nsinc			# sinc truncation length
real	mindx			# interpolation minimum

int	n
real	x, y, dx, x1, x2

begin
	x1 = min (a, b)
	x2 = max (a, b)
	n = max (1, nint (x2 - x1))
	dx = (x2 - x1) / n

	sum = 0.
	for (x = x1 + dx / 2; x < x2; x = x + dx) {
	    call ii_sinc (x, y, 1, data, npix, nsinc, mindx)
	    sum = sum + y * dx
	}
end


# II_DZIGRL -- Procedure to integrate the drizzle interpolant.

procedure ii_dzigrl (a, b, sum, data, pixfrac)

real    a, b         	# x start and stop values, must be within [1,npts]
real    sum		# integgral value returned to the user
real    data[ARB]       # data to be interpolated
real    pixfrac         # the drizzle pixel fraction

int     j, neara, nearb
real    hpixfrac, xa, xb, dx, accum

begin
        hpixfrac = pixfrac / 2.0

        # Define the interval of integration.
        xa = min (a, b)
        xb = max (a, b)
        neara = xa + 0.5
        nearb = xb + 0.5

        # Initialize the integration
        accum = 0.0
        if (neara == nearb) {

            dx = min (xb, nearb + hpixfrac) - max (xa, neara - hpixfrac)
            if (dx > 0.0)
                accum = accum + dx * data[neara]

        } else {

            # first segement
            dx = neara + hpixfrac - max (xa, neara - hpixfrac)
            if (dx > 0.0)
                accum = accum + dx * data[neara]

            # interior segments.
            do j = neara + 1, nearb - 1
                accum = accum + pixfrac * data[j]

            # last segment
            dx = min (xb, nearb + hpixfrac) - (nearb - hpixfrac)
            if (dx > 0.0)
                accum = accum + dx * data[nearb]
        }

        if (a > b)
            sum = -accum
        else
            sum = accum 
end


# II_DZIGRL1 -- Procedure to integrate the drizzle interpolant in the case
# where pixfrac = 1.0.

procedure ii_dzigrl1 (a, b, sum, data)

real    a, b            # x start and stop values, must be within [1,npts]
real    sum		# integgral value returned to the user
real    data[ARB]       # data to be interpolated

int     j, neara, nearb
real    xa, xb, deltaxa, deltaxb, accum

begin
        # Define the interval of integration.
        xa = min (a, b)
        xb = max (a, b)
        neara = xa + 0.5
        nearb = xb + 0.5
        deltaxa = xa - neara
        deltaxb = xb - nearb

        # Only one segment involved.
        accum = 0.0
        if (neara == nearb) {

            accum = accum + (deltaxb - deltaxa) * data[neara]

        } else {

            # First segment.
            accum = accum + (0.5 - deltaxa) * data[neara]

            # Middle segment.
            do j = neara + 1, nearb - 1
                accum = accum + data[j]

            # Last segment.
            accum = accum + (deltaxb + 0.5) * data[nearb]
        }

        if (a > b)
           sum = -accum 
        else
           sum = accum
end

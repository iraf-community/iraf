# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include "im1interpdef.h"

# IA_PCPOLY3 -- Calculate the coefficients of a 3rd order polynomial.

procedure ia_pcpoly3 (x, datain, npts, pcoeff)

real	x		# x value
real	datain[ARB]	# array of input data
int	npts		# number of data points
real	pcoeff[ARB]	# array of polynomial coefficients

int 	i, k, nearx, nterms
real	temp[POLY3_ORDER]

begin
	nearx = x

	# Check for edge problems.
	k = 0
	for(i = nearx - 1; i <= nearx + 2; i = i + 1) {
	    k = k + 1

	    # project data points into temporary array
	    if (i < 1)
		temp[k] = 2. * datain[1] - datain[2-i]
	    else if (i > npts)
		temp[k] = 2. * datain[npts] - datain[2*npts-i]
	    else
		temp[k] = datain[i]
	}

	nterms = 4

	# Generate the difference table for Newton's form.
	do k = 1, nterms - 1
	    do i = 1, nterms - k
		temp[i] = (temp[i+1] - temp[i]) / k
	
	# Shift to generate polynomial coefficients.
	do k = nterms, 2, -1
	    do i = 2, k
		temp[i] = temp[i] + temp[i-1] * (k- i - nterms/2)
	do i = 1, nterms
	    pcoeff[i] = temp[nterms+1-i]
end


# IA_PCPOLY5 -- Calculate the coefficients of a fifth order polynomial.

procedure ia_pcpoly5 (x, datain, npts, pcoeff)

real	x		# x value
real	datain[ARB]	# array of input data
int	npts		# number of data points
real	pcoeff[ARB]	# array of polynomial coefficients

int 	i, k, nearx, nterms
real	temp[POLY5_ORDER]

begin
	nearx = x

	# Check for edge effects.
	k = 0
	for (i = nearx - 2; i <= nearx + 3; i = i + 1) {
	    k = k + 1
	    # project data points into temporary array
	    if (i < 1)
		temp[k] = 2. * datain[1] - datain[2-i]
	    else if (i > npts)
		temp[k] = 2. * datain[npts] - datain[2*npts-i]
	    else
		temp[k] = datain[i]
	}

	nterms = 6

	# Generate difference table for Newton's form.
	do k = 1, nterms - 1
	    do i = 1, nterms - k
		temp[i] = (temp[i+1] - temp[i]) / k
	
	# Shift to generate polynomial coefficients.
	do k = nterms, 2, -1
	    do i = 2, k
		temp[i] = temp[i] + temp[i-1] * (k - i - nterms/2)
	do i = 1, nterms
	    pcoeff[i] = temp[nterms+1-i]
end


# IA_PCSPLINE3 -- Calculate the derivatives of a cubic spline.

procedure ia_pcspline3 (x, datain, npts, pcoeff)

real	x		# x value
real	datain[ARB]	# data array
int	npts		# number of data points
real	pcoeff[ARB]	# array of polynomial coefficients

int 	i, k, nearx, px
real	temp[SPLPTS+3], bcoeff[SPLPTS+3]

begin
	nearx = x
	k = 0

	# Check for edge effects.
	for (i = nearx - SPLPTS/2 + 1; i <= nearx + SPLPTS/2; i = i + 1) {
	    if(i < 1 || i > npts)
		;
	    else {
		k = k + 1
		if (k == 1)
		    px = nearx - i + 1
		bcoeff[k+1] = datain[i]
	    }
	}

	bcoeff[1] = 0.
	bcoeff[k+2] = 0.

	# Use special routine for cardinal splines.
	call ii_spline (bcoeff, temp, k)

	px = px + 1
	bcoeff[k+3] = 0.

	# Calculate polynomial coefficients.
	pcoeff[1] = bcoeff[px-1] + 4. * bcoeff[px] + bcoeff[px+1]
	pcoeff[2] = 3. * (bcoeff[px+1] - bcoeff[px-1])
	pcoeff[3] = 3. * (bcoeff[px-1] - 2. * bcoeff[px] + bcoeff[px+1])
	pcoeff[4] = -bcoeff[px-1] + 3. * bcoeff[px] - 3. * bcoeff[px+1] +
				    bcoeff[px+2]
end


# II_SINCDER -- Evaluate derivatives of the sinc interpolator. If the
# function value only is needed call ii_sinc. This routine computes only
# the first two derivatives. The second  derivative is computed even if only
# the first derivative is needed. The sinc truncation length is nsinc.
# The tape is a triangle function of slope staper beginning at ntaper.
# The data value is returned if x is closer to x[i] than mindx.

procedure ii_sincder (x, der, nder, data, npix, nsinc, ntaper, staper, mindx)

real	x		# x value
real	der[ARB]	# derivatives to return
int	nder		# number of derivatives
real	data[npix]	# data to be interpolated
int	npix		# number of pixels
int	nsinc		# sinc truncation length
int	ntaper		# start of triangular taper
real	staper		# slope of triangular taper
real	mindx		# interpolation minimum

int	i, j, xc
real	dx, w, a, d, z
real	w1, w2, w3, u1, u2, u3, v1, v2, v3, u1a, u2a, u3a, v1a, v2a, v3a

begin
	# Return if no derivatives.
	if (nder == 0)
	    return

	# Set derivatives intially to zero.
	do i = 1, nder
	    der[i] = 0.

	# Return if outside data range.
	xc = nint (x)
	if (xc < 1 || xc > npix)
	    return

	# Call ii_sinc if only the function value is needed.
	if (nder == 1) {
	    call ii_sinc (x, der, 1, data, npix, nsinc, ntaper, staper, mindx)
	    return
	}

	# Compute the derivatives.
	dx = x - xc
	if (abs (dx) < mindx) {

	    w = 1.

	    d = data[xc]
	    w1 = 1.; u1 = d * w1; v1 = w1
	    w2 = 0.; u2 = 0.; v2 = 0.
	    w3 = -1./3.; u3 = d * w3; v3 = w3

	    do i = 1, nsinc {

		w = -w
		if (i > ntaper) {
		    if (w < 0.)
			w = min (0., w + staper)
		    else
			w = max (0., w - staper)
		    if (w == 0.)
			break
		}

		u1a = u1; v1a = v1; u2a = u2; v2a = v2; u3a = u3; v3a = v3

		j = xc - i
		z = 1. / i
		if (j >= 1) {
		    d = data[j]
		    w2 = w * z; u2 = u2 + d * w2; v2 = v2 + w2
		    w3 = -2 * w2 * z; u3 = u3 + d * w3; v3 = v3 + w3
		}

		j = xc + i
		if (j <= npix) {
		    d = data[j]
		    w2 = -w * z; u2 = u2 + d * w2; v2 = v2 + w2
		    w3 = 2 * w2 * z; u3 = u3 + d * w3; v3 = v3 + w3
		}
	    }

	} else {

	    w = 1.
	    a = 1 / tan (PI * dx)

	    d = data[xc]
	    z = 1. / dx
	    w1 = w * z; u1 = d * w1; v1 = w1
	    w2 = w1 * (a - z); u2 = d * w2; v2 = w2
	    w3 = -w1 * (1 + 2 * z * (a - z)); u3 = d * w3; v3 = w3

	    do i = 1, nsinc {

		w = -w
		if (i > ntaper) {
		    if (w < 0.)
			w = min (0., w + staper)
		    else
			w = max (0., w - staper)
		    if (w == 0.)
			break
		}

		u1a = u1; v1a = v1; u2a = u2; v2a = v2; u3a = u3; v3a = v3

		j = xc - i
		if (j >= 1) {
		    d = data[j]
		    z = 1. / (dx + i)
		    w1 = w * z; u1 = u1 + d * w1; v1 = v1 + w1
		    w2 = w1 * (a - z); u2 = u2 + d * w2; v2 = v2 + w2
		    w3 = -w1 * (1 + 2*z*(a-z)); u3 = u3 + d * w3; v3 = v3 + w3
		}

		j = xc + i
		if (j <= npix) {
		    d = data[j]
		    z = 1. / (dx - i)
		    w1 = w * z; u1 = u1 + d * w1; v1 = v1 + w1
		    w2 = w1 * (a - z); u2 = u2 + d * w2; v2 = v2 + w2
		    w3 = -w1 * (1 + 2*z*(a-z)); u3 = u3 + d * w3; v3 = v3 + w3
		}
	    }
	}

	w1 = v1
	w2 = v1 * w1
	w3 = v1 * w2
	der[1] = u1 / w1
	if (nder > 1)
	    der[2] = (u2 * v1 - u1 * v2) / w2
	if (nder > 2)
	    der[3] = u3 / w1 - 2*u2*v2 / w2 + 2*u1*v2*v2 / w3 - u1*v3 / w2

	w1 = v1a
	w2 = v1a * w1
	w3 = v1a * w2
	der[1] = (der[1] + u1a / w1) / 2.
	if (nder > 1)
	    der[2] = (der[2] + (u2a * v1a - u1a * v2a) / w2) / 2.
	if (nder > 2)
	    der[3] = (der[3] + u3a / w1 - 2*u2a*v2a / w2 +
		2*u1a*v2a*v2a / w3 - u1a*v3a / w2) / 2.
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"

# IA_PCPOLY3 -- procedure to calculate the coefficients of a 3rd order
# polynomial

procedure ia_pcpoly3 (x, datain, npts, pcoeff)

real	x		# x value
real	datain[ARB]	# array of input data
int	npts		# number of data points
real	pcoeff[ARB]	# array of polynomial coefficients

int 	i, k, nearx, nterms
real	temp[POLY3_ORDER]

begin
	nearx = x

	# The major complication is that near the edge interior polynomial
	# must somehow be defined.

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

	# generate diffrence table for Newton's form
	do k = 1, nterms - 1
	    do i = 1, nterms - k
		temp[i] = (temp[i+1] - temp[i]) / k
	
	# shift to generate polynomial coefficients
	do k = nterms, 2, -1
	    do i = 2, k
		temp[i] = temp[i] + temp[i-1] * (k- i - nterms/2)
	
	do i = 1, nterms
	    pcoeff[i] = temp[nterms+1-i]
end


# IA_PCPOLY5 -- procedure to calculate the coefficients of a fifth order
# polynomial

procedure ia_pcpoly5 (x, datain, npts, pcoeff)

real	x		# x value
real	datain[ARB]	# array of input data
int	npts		# number of data points
real	pcoeff[ARB]	# array of polynomial coefficients

int 	i, k, nearx, nterms
real	temp[POLY5_ORDER]

begin
	nearx = x

	# The major complication is that near the edge interior polynomial
	# must somehow be defined.

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

	# generate difference table for Newton's form
	do k = 1, nterms - 1
	    do i = 1, nterms - k
		temp[i] = (temp[i+1] - temp[i]) / k
	
	# shift to generate polynomial coefficients
	do k = nterms, 2, -1
	    do i = 2, k
		temp[i] = temp[i] + temp[i-1] * (k - i - nterms/2)
	
	do i = 1, nterms
	    pcoeff[i] = temp[nterms+1-i]
end


# IA_PCSPLINE3 -- procedure to calculate the derivatives of a cubic spline

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

	# maximum number of points used is SPLPTS
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

	# calculate polynomial coefficients
	pcoeff[1] = bcoeff[px-1] + 4. * bcoeff[px] + bcoeff[px+1]
	pcoeff[2] = 3. * (bcoeff[px+1] - bcoeff[px-1])
	pcoeff[3] = 3. * (bcoeff[px-1] - 2. * bcoeff[px] + bcoeff[px+1])
	pcoeff[4] = -bcoeff[px-1] + 3. * bcoeff[px] - 3. * bcoeff[px+1] +
				    bcoeff[px+2]
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		procedure asider

     This procedure finds derivatives assuming that x lands in
array i.e. 1 <= x <= npts.
It must be called by a routine that checks for out of bound references
and takes care of bad points and checks to make sure that the number
of derivatives requested is reasonable (max. is 5 derivatives or nder
can be 6 at most and min. nder is 1)

     This version assumes interior polynomial interpolants are stored as
the data points with corections for bad points, and the spline interpolant
is stored as a series of b-spline coefficients.

     The storage and evaluation for nearest neighbor and linear interpolation
are simpler so they are evaluated separately from other piecewise
polynomials.
.endhelp

procedure asider(x,der,nder,coeff)
include "interpdef.h"
include "asidef.h"

real x
real coeff[ARB]
int nder		# no. items returned = 1 + no. of deriv.

real der[ARB]		# der[1] is value der[2] is f prime etc.

int nx,n0,i,j,k,nt,nd
real s,ac,pc[6],d[6]

begin
	do i = 1, nder   # return zero for derivatives that are zero
	    der[i] = 0.

	nt = 0    # nt is number of terms in case polynomial type

	switch (ITYPEI)	{	# switch on interpolator type
	
	case IT_NEAREST :
	    der[1] = (coeff[COFF + nint(x)])
	    return

	case IT_LINEAR :
	    nx = x
	    der[1] = (x - nx) * coeff[COFF + nx + 1] +
		(nx + 1 - x) * coeff[COFF + nx]
	    if ( nder > 1 )		# try to return exact number requested
		der[2] = coeff[COFF + nx + 1] - coeff[COFF + nx]
	    return

	case IT_POLY3 :
	    nt = 4

	case IT_POLY5 :
	    nt = 6

	case IT_SPLINE3 :
	    nt = 4

	}

	# falls through to here if interpolant is one of
	# the higher order polynomial types or third order spline

	nx = x
	n0 = COFF + nx
	s = x - nx

	nd = nder		# no. of derivatives needed
	if ( nder > nt )
	    nd = nt

	# generate polynomial coefficients, first for spline.
	if (ITYPEI == IT_SPLINE3) {
	    pc[1] = coeff[n0-1] + 4. * coeff[n0] + coeff[n0+1]
	    pc[2] = 3. * (coeff[n0+1] - coeff[n0-1])
	    pc[3] = 3. * (coeff[n0-1] - 2. * coeff[n0] + coeff[n0+1])
	    pc[4] = -coeff[n0-1] + 3. * coeff[n0] - 3. * coeff[n0+1] +
				    coeff[n0+2]
		
	} else {

	# Newton's form written in line to get polynomial from data
	    # load data
	    do i = 1,nt
		d[i] = coeff[n0 - nt/2 + i]

	    # generate difference table
	    do k = 1, nt-1
		do i = 1,nt-k
		    d[i] = (d[i+1] - d[i]) / k

	    # shift to generate polynomial coefficients of (x - n0)
	    do k = nt,2,-1
		do i = 2,k
		    d[i] = d[i] + d[i-1] * (k - i - nt/2)

	    do i = 1,nt
		pc[i] = d[nt + 1 - i]
	}

	do k = 1,nd {  # as loop progresses pc contains coefficients of
			# higher and higher derivatives

	    ac = pc[nt - k + 1]
	    do j = nt - k, 1, -1	# evaluate using nested mult.
		ac = pc[j] + s * ac

	    der[k] = ac

	    do j = 1,nt - k 	# differentiate polynomial
		pc[j] = j * pc[j + 1]
	}

	return

end

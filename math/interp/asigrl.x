# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		procedure asigrl

     This procedure finds the integral of the interpolant from a to b
assuming both a and b land in the array.
.endhelp

real procedure asigrl(a,b,coeff)    # returns value of integral
include "interpdef.h"
include "asidef.h"

real a,b		# integral limits
real coeff[ARB]

int na,nb,i,j,n0,nt
real s,t,ac,xa,xb,pc[6]

begin
	xa = a
	xb = b
	if ( a > b ) {		# flip order and sign at end
	    xa = b
	    xb = a
	}

	na = xa
	nb = xb
	ac = 0.		# zero accumulator

	# set number of terms
	switch (ITYPEI) {		# switch on interpolator type
	case IT_NEAREST :
	    nt = 0
	case IT_LINEAR :
	    nt = 1
	case IT_POLY3 :
	    nt = 4
	case IT_POLY5 :
	    nt = 6
	case IT_SPLINE3 :
	    nt = 4
	}

	# NEAREST_NEIGHBOR and LINEAR are handled differently because of
	# storage.  Also probably good for speed.

	if (nt == 0) {		# NEAREST_NEIGHBOR
	    # reset segment to center values
	    na = xa + 0.5
	    nb = xb + 0.5

	    # set up for first segment
	    s = xa - na

	    # for clarity one segment case is handled separately
	    if ( nb == na ) {	# only one segment involved
		t = xb - nb
		n0 = COFF + na
		ac = ac + (t - s) * coeff[n0]
	    } else {	# more than one segment

		# first segment
		n0 = COFF + na
		ac = ac + (0.5 - s) * coeff[n0]

		# middle segments
		do j = na+1, nb-1 {
		    n0 = COFF + j
		    ac = ac + coeff[n0]
		}

		# last segment
		n0 = COFF + nb
		t = xb - nb
		ac = ac + (t + 0.5) * coeff[n0]
	    }

	} else if (nt == 1) {	# LINEAR
	    # set up for first segment
	    s = xa - na

	    # for clarity one segment case is handled separately
	    if ( nb == na ) {	# only one segment involved
		t = xb - nb
		n0 = COFF + na
		ac = ac + (t - s) * coeff[n0] +
		     0.5 * (coeff[n0+1] - coeff[n0]) * (t*t - s*s)
	    } else {	# more than one segment

		# first segment
		n0 = COFF + na
		ac = ac + (1. - s) * coeff[n0] +
		     0.5 * (coeff[n0+1] - coeff[n0]) * (1. - s*s)

		# middle segments
		do j = na+1, nb-1 {
		    n0 = COFF + j
		    ac = ac + 0.5 * (coeff[n0+1] + coeff[n0])
		}

		# last segment
		n0 = COFF + nb
		t = xb - nb
		ac = ac + coeff[n0] * t + 0.5 *
			(coeff[n0+1] - coeff[n0]) * t * t
	    }

	} else {		# A higher order interpolant

	    # set up for first segment
	    s = xa - na

	    # for clarity one segment case is handled separately
	    if ( nb == na ) {	# only one segment involved
		t = xb - nb
		n0 = COFF + na
		call iigetpc(n0,pc,coeff)
		do i = 1,nt
		    ac = ac + (1./i) * pc[i] * (t ** i - s ** i)
	    } else {	# more than one segment

		# first segment
		n0 = COFF + na
		call iigetpc(n0,pc,coeff)
		do i = 1,nt
		    ac = ac + (1./i) * pc[i] * (1. - s ** i)

		# middle segments
		do j = na+1, nb-1 {
		    n0 = COFF + j
		    call iigetpc(n0,pc,coeff)
		    do i = 1,nt
			ac = ac + (1./i) * pc[i]
		}

		# last segment
		n0 = COFF + nb
		t = xb - nb
		call iigetpc(n0,pc,coeff)
		do i = 1,nt
		    ac = ac + (1./i) * pc[i] * t ** i
	    }
	}

	if ( a < b )
	    return(ac)
	else
	    return(-ac)
end

	
procedure iigetpc(n0, pc, coeff)	# generates polynomial coefficients
					# if spline or poly3 or poly5

int n0			# coefficients wanted for n0 < x n0 + 1
real coeff[ARB]

real pc[ARB]

int i,k,nt
real d[6]

begin
	# generate polynomial coefficients, first for spline.
	if (ITYPEI == IT_SPLINE3) {
	    pc[1] = coeff[n0-1] + 4. * coeff[n0] + coeff[n0+1]
	    pc[2] = 3. * (coeff[n0+1] - coeff[n0-1])
	    pc[3] = 3. * (coeff[n0-1] - 2. * coeff[n0] + coeff[n0+1])
	    pc[4] = -coeff[n0-1] + 3. * coeff[n0] - 3. * coeff[n0+1] +
				    coeff[n0+2]
		
	} else {
	    if (ITYPEI == IT_POLY5)
		nt = 6
	    else	# must be POLY3
		nt = 4

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
		    d[i] = d[i] + d[i-1] * (k - i - 2)

	    do i = 1,nt
		pc[i] = d[nt + 1 - i]
	}

	return
end

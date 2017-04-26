include <mach.h>
include <math.h>
include "rvpackage.h"
include "rvflags.h"

# RV_SINC - Do the Fourier (sinc) interpolation to determine the peak center
# and FWHM values.  Height of the ccf at the peak is also returned.

procedure rv_sinc (rv, shift, fwhm, height)

pointer	rv					#I RV struct pointer
real	shift					#O Shift of the peak
real	fwhm					#O FWHM of the peak
real	height					#O Height of peak at center

int	i, j, k, il, ir
real	x, y, back, lhp, rhp, hpower, ipeak
real 	brent(), sinc_interp(), rv_maxpix()

errchk	realloc, mfree

include	"rvsinc.com"

begin
	# Initialize.
	il = RV_ISTART(rv)
	ir = RV_IEND(rv)
	ipeak = WRKPIXX(rv,RV_ISHIFT(rv))
	snfit = ir - il + 1

	# Allocate the pointers in the common
	call realloc (sx, snfit, TY_REAL)
	call realloc (sy, snfit, TY_REAL)
	call realloc (splx, snfit*10, TY_REAL)
	call realloc (sply, snfit*10, TY_REAL)

	# Now move the part of the ccf we're fitting into the arrays, but
	# change the sign of the ccf because we're finding a minimum with 
	# the algorithm used
	call amovr (WRKPIXX(rv,il), Memr[sx], snfit)
	call amulkr (WRKPIXY(rv,il), -1.0, Memr[sy], snfit)

	# Now find the peak center and height.
	height = brent (ipeak-1., ipeak, ipeak+1., RV_TOLERANCE(rv), 
	    RV_MAXITERS(rv), shift)
	height = - height

	# Compute the sinc interpolant over the ccf to see how close we came.
	# It will be plotted later so we don't free the pointers right away.
	do i = 1, snfit-1 {
	    do j = 1, 10 {
   	        k = ((i-1)*10+j)
   	        Memr[splx+k-1] = Memr[sx+i-1] + ((j-1)/10.)
	        Memr[sply+k-1] = - sinc_interp (Memr[splx+k-1])
	    }
	}
	height = rv_maxpix (Memr[sply], snfit*10)

	# Compute the FWHM through some rather brute force methods.
	back = RV_BACKGROUND(rv)
	hpower = back + (height - back) / 2.
	if (IS_INDEF(RV_BACKGROUND(rv))) {
	    fwhm = INDEF			# don't compute a fwhm
	    RV_FWHM_Y(rv) = INDEF
        } else if (WRKPIXY(rv,RV_ISTART(rv)) > hpower ||
                   WRKPIXY(rv,RV_IEND(rv)) > hpower) {
	    fwhm = INDEF			# don't compute a fwhm
	    RV_FWHM_Y(rv) = INDEF
	} else {
	    RV_FWHM_Y(rv) = hpower
	    for (i=RV_ISHIFT(rv); WRKPIXY(rv,i)>hpower&&i>=1; i=i-1)
	        x = WRKPIXX(rv,i)
	    for (y=-sinc_interp(x); abs(y-hpower)>0.005; x=x-0.005)
	        y = - sinc_interp (x-0.005);
	    lhp = x - 0.005
	    for (i=RV_ISHIFT(rv); WRKPIXY(rv,i)>hpower && i<=RV_CCFNPTS(rv);
	        i=i+1)
	    x = WRKPIXX(rv,i)
	    for (y=-sinc_interp(x); abs(y-hpower)>0.005; x=x+0.005)
	        y = - sinc_interp (x+0.005);
	    rhp = x + 0.005
            fwhm = abs (rhp - lhp)
	}

	# Clean up a little
	call mfree (sx, TY_REAL)
	call mfree (sy, TY_REAL)
end


# SINC_INTERP - Function subroutine to do the sine (fourier) interpolation and
# return the value of the correlation function for any x value.
#
#     h(t) = Sum(all n) [ h_n * sin(pi*(t-n)) /(pi*(t-n))]
#
# The interval between samples is assumed to be unity.

real procedure sinc_interp (x)

real	x					#I Point to be evaluated

real 	sval, tmp
int	i

include	"rvsinc.com"

begin
	# Check for an integer x.  If present, return the ccf value and don't
	# interpolate.
	tmp = abs (float(nint(x)) - x)
	if (tmp < EPSILON && x >= Memr[sx] && x <= Memr[sx+snfit-1]) {
	    do i = 1, snfit {
		if (abs(Memr[sx+i-1]-x) < EPSILON)	# find the y point
	            sval = Memr[sy+i-1]
	    }
	    return (sval)
	}

	# Do the evaluation.
	tmp = sin (PI*(x-Memr[sx]))
	sval = Memr[sy] * tmp / (PI * (x - Memr[sx]))
	do i = 1, snfit-1 {
	    tmp = -tmp
	    sval = sval + Memr[sy+i] * tmp / (PI * (x - Memr[sx+i]))
	}

	return (sval)
end


# BRENT - Given a function F(), and given a bracketing triplet of abscissas
# AX, BX, CX (such that BX is between AX and CX, and F(bx) is less than both
# F(AX) and F(BX)), this routine isolates the minimum to a fractional precision
# of about TOL using Brent's Method.  The abscissa of the minimum is returned
# as XMIN, and the minimum function value is the function return value.

real procedure brent (ax, bx, cx, tol, itmax, xmin)

real	ax, bx, cx				#I Interp. bracketing points
real	tol					#I Tolerance
int	itmax					#I Max no. of iterations
real	xmin					#O Minimum point

real	a, b, d, etemp, fu, fv, fw, fx		# local variables
real	p, q, r, tol1, tol2, u, v, w, x, xm
real	e
int	iter

real	sinc_interp()

define	CGOLD		.3819660		# golden ration
define	ZEPS		1.0e-10			# a small number
define	SHIFT		{$1=$2;$2=$3;$3=$4}

begin
	a = min (ax, cx)			# initialize
	b = max (ax, cx)
	v = bx
	w = v
	x = v
	e = 0.
	fx = sinc_interp (x)
	fv = fx
	fw = fx
	e = 0.0
	
	do iter = 1, itmax {
	    xm = 0.5 * (a + b)
	    tol1 = tol * abs (x) + ZEPS
	    tol2 = 2. * tol1
	    if (abs(x-xm) <= (tol2-0.5*(b-a))) {	# test for convergence
		xmin = x
		return (fx)
	    }
	    if (abs(e) > tol1) {			# construct a trial
	        r = (x-w) * (fx-fv) 			# parabolic fit
	        q = (x-v) * (fx-fw)
	        p = (x-v) * q - (x-w) * r
	        q = 2. * (q-r)
	        if (q > 0.) 
		    p = -p
	        q = abs(q)
	        etemp = e
	        e = d

		# Determine the acceptability of the parabolic fit.  Here we
		# take the golden section step into the larger of the two
		# segments.

	        if (abs(p) >= abs(.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x)) {
       		    if (x >= xm)
	    	        e = a - x
	  	    else
	    	        e = b - x
	  	    d = CGOLD * e
		} else {
	            d = p/q				 # take parabolic step
	            u = x+d
	            if (u-a < tol2 || b-u < tol2) 
			d = sign (tol1, xm-x)
		}
	    } else {
       		if (x >= xm)
	    	    e = a - x
	  	else
	    	    e = b - x
	  	d = CGOLD * e
	    }

	    if (abs(d) >= tol1)
	        u = x + d
	    else
	        u = x + sign (tol1,d)

	    fu = sinc_interp (u)
	    if (fu <= fx) {
	        if (u >= x)
	            a = x
	        else
	            b = x
		SHIFT(v,w,x,u)
		SHIFT(fv,fw,fx,fu)
	    } else {
	        if (u < x)
	            a = u
	        else
	            b = u
	        if (fu <= fw || w == x) {
	            v = w
	            fv = fw
	            w = u
	            fw = fu
	        } else if (fu <= fv || v == x || v == w) {
	            v = u
	            fv = fu
	        }
	    }
	}

	call rv_errmsg ("BRENT: Exceeded maximum number of iterations.\n")
        xmin = x
	return (fx)
end

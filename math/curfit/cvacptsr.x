# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/curfit.h>

include "curfitdef.h"

# CVACPTS -- Procedure to add a set of points to the normal equations.
# The inner products of the basis functions are calculated and
# accumulated into the CV_ORDER(cv) by CV_NCOEFF(cv) matrix MATRIX.
# The main diagonal of the matrix is stored in the first row of
# MATRIX followed by the remaining non-zero diagonals. This method
# of storage is particularly efficient for the large symmetric
# banded matrices produced during spline fits. The inner product
# of the basis functions and the data ordinates are stored in the
# CV_NCOEFF(cv)-vector VECTOR. The array LEFT stores the
# indices which show which elements of MATRIX and VECTOR are
# to receive the inner products.

procedure cvacpts (cv, x, y, w, npts, wtflag)

pointer	cv		# curve descriptor
real	x[npts]		# array of abcissa
real	y[npts]		# array of ordinates
real	w[npts]		# array of weights
int	npts		# number of data points
int	wtflag		# type of weighting

int	i, ii, j, k
pointer	sp
pointer	vzptr, vindex, mzptr, mindex, bptr, bbptr
pointer	bw, rows

begin

	# increment the number of points
	CV_NPTS(cv) = CV_NPTS(cv) + npts

	# remove basis functions calculated by any previous cvrefit call
	if (CV_BASIS(cv) != NULL) {

	    call mfree (CV_BASIS(cv), TY_REAL)
	    call mfree (CV_WY(cv), TY_REAL)

	    CV_BASIS(cv) = NULL
	    CV_WY(cv) = NULL

	    if (CV_LEFT(cv) != NULL) {
	        call mfree (CV_LEFT(cv), TY_INT)
	        CV_LEFT(cv) = NULL
	    }
	}

	# calculate weights
	switch (wtflag) {
	case WTS_UNIFORM:
	    call amovkr (real(1.0), w, npts)
	case WTS_SPACING:
	    if (npts == 1)
		w[1] = 1.
	    else
	        w[1] = abs (x[2] - x[1])
	    do i = 2, npts - 1
		w[i] = abs (x[i+1] - x[i-1])
	    if (npts == 1)
		w[npts] = 1.
	    else
	        w[npts] = abs (x[npts] - x[npts-1])
	case WTS_USER:
	    # user supplied weights
	case WTS_CHISQ:
	    # data assumed to be scaled to photons with Poisson statistics
	    do i = 1, npts {
		if (y[i] > real(0.0))
		    w[i] = real(1.0) / y[i]
		else if (y[i] < real(0.0))
		    w[i] = -real(1.0) / y[i]
		else
		    w[i] = real(0.0)
	    }
	default:
	    call amovkr (real(1.0), w, npts)
	}


	# allocate space for the basis functions
	call smark (sp)
	call salloc (CV_BASIS(cv), npts * CV_ORDER(cv), TY_REAL)

	# calculate the non-zero basis functions
	switch (CV_TYPE(cv)) {
	case LEGENDRE:
	    call rcv_bleg (x, npts, CV_ORDER(cv), CV_MAXMIN(cv),
	    		  CV_RANGE(cv), BASIS(CV_BASIS(cv)))
	case CHEBYSHEV:
	    call rcv_bcheb (x, npts, CV_ORDER(cv), CV_MAXMIN(cv),
	    		  CV_RANGE(cv), BASIS(CV_BASIS(cv)))
	case SPLINE3:
	    call salloc (CV_LEFT(cv), npts, TY_INT)
	    call rcv_bspline3 (x, npts, CV_NPIECES(cv), -CV_XMIN(cv),
			      CV_SPACING(cv), BASIS(CV_BASIS(cv)),
			      LEFT(CV_LEFT(cv)))
	case SPLINE1:
	    call salloc (CV_LEFT(cv), npts, TY_INT)
	    call rcv_bspline1 (x, npts, CV_NPIECES(cv), -CV_XMIN(cv),
			      CV_SPACING(cv), BASIS(CV_BASIS(cv)),
			      LEFT(CV_LEFT(cv)))
	case USERFNC:
	    call rcv_buser (cv, x, npts)
	}


	# allocate temporary storage space for matrix accumulation
	call salloc (bw, npts, TY_REAL)
	call salloc (rows, npts, TY_INT)

	# one index the pointers
	vzptr = CV_VECTOR(cv) - 1
	mzptr = CV_MATRIX(cv)
	bptr = CV_BASIS(cv)

	switch (CV_TYPE(cv)) {

	case LEGENDRE, CHEBYSHEV, USERFNC:

	    # accumulate the new right side of the matrix equation
	    do k = 1, CV_ORDER(cv) {
	        call amulr (w, BASIS(bptr), Memr[bw], npts) 
		vindex = vzptr + k
	        do i = 1, npts
	            VECTOR(vindex) = VECTOR(vindex) + Memr[bw+i-1] * y[i]
	        bbptr = bptr
	        ii = 0
	        do j = k, CV_ORDER(cv) {
		    mindex = mzptr + ii
		    do i = 1, npts
		        MATRIX(mindex) = MATRIX(mindex) + Memr[bw+i-1] *
				            BASIS(bbptr+i-1)	
		    ii = ii + 1
		    bbptr = bbptr + npts
		}
		bptr = bptr + npts
		mzptr = mzptr + CV_ORDER(cv)
	    }

	case SPLINE1,SPLINE3:

	    call amulki (LEFT(CV_LEFT(cv)), CV_ORDER(cv), Memi[rows], npts)
	    call aaddki (Memi[rows], CV_MATRIX(cv), Memi[rows], npts)
	    call aaddki (LEFT(CV_LEFT(cv)), vzptr, LEFT(CV_LEFT(cv)), npts) 

	    # accumulate the new right side of the matrix equation
	    do k = 1, CV_ORDER(cv) {
	        call amulr (w, BASIS(bptr), Memr[bw], npts) 
	        do i = 1, npts {
		    vindex = LEFT(CV_LEFT(cv)+i-1) + k
	            VECTOR(vindex) = VECTOR(vindex)+ Memr[bw+i-1] * y[i]
		}
	        bbptr = bptr
	        ii = 0
	        do j = k, CV_ORDER(cv) {
		    do i = 1, npts {
			mindex = Memi[rows+i-1] + ii
		        MATRIX(mindex) = MATRIX(mindex) + Memr[bw+i-1] *
			    BASIS(bbptr+i-1)
		    }
		    ii = ii + 1
		    bbptr = bbptr + npts
		}
	        bptr = bptr + npts
	        call aaddki (Memi[rows], CV_ORDER(cv), Memi[rows], npts)
	    }
	}

	# release the space
	call sfree (sp)
	CV_BASIS(cv) = NULL
	CV_LEFT(cv) = NULL
end

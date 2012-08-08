# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "gsurfitdef.h"

# GSACPTS -- Procedure to add a set of points to the normal equations.
# The inner products of the basis functions are calculated and
# accumulated into the GS_NCOEFF(sf) ** 2 matrix MATRIX.
# The main diagonal of the matrix is stored in the first row of
# MATRIX followed by the remaining non-zero diagonals.
# The inner product
# of the basis functions and the data ordinates are stored in the
# NCOEFF(sf)-vector VECTOR.

procedure gsacpts (sf, x, y, z, w, npts, wtflag)

pointer	sf		# surface descriptor
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	z[npts]		# data array
real	w[npts]		# array of weights
int	npts		# number of data points
int	wtflag		# type of weighting

bool	refsub
int	i, ii, j, jj, k, l, ll
int	maxorder, xorder, xxorder, ntimes
pointer	sp, vzptr, vindex, mzptr, mindex, bxptr, bbxptr, byptr, bbyptr
pointer	x1, y1, z1, byw, bw

real	adotr()

begin
	# increment the number of points
	GS_NPTS(sf) = GS_NPTS(sf) + npts

	# remove basis functions calculated by any previous gsrefit call
	if (GS_XBASIS(sf) != NULL || GS_YBASIS(sf) != NULL) {

	    if (GS_XBASIS(sf) != NULL)
	        call mfree (GS_XBASIS(sf), TY_REAL)
	    GS_XBASIS(sf) = NULL
	    if (GS_YBASIS(sf) != NULL)
	        call mfree (GS_YBASIS(sf), TY_REAL)
	    GS_YBASIS(sf) = NULL
	    if (GS_WZ(sf) != NULL)
	        call mfree (GS_WZ(sf), TY_REAL)
	    GS_WZ(sf) = NULL
	}

	# calculate weights
	switch (wtflag) {
	case WTS_UNIFORM:
	    call amovkr (1., w, npts)
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
	default:
	    call amovkr (1., w, npts)
	}


	# allocate space for the basis functions
	call smark (sp)
	call salloc (GS_XBASIS(sf), npts * GS_XORDER(sf), TY_REAL)
	call salloc (GS_YBASIS(sf), npts * GS_YORDER(sf), TY_REAL)

	# subtract reference value
	refsub = !(IS_INDEFR(GS_XREF(sf)) || IS_INDEFR(GS_YREF(sf)) ||
	    IS_INDEFR(GS_ZREF(sf)))
	if (refsub) {
	    call salloc (x1, npts, TY_REAL)
	    call salloc (y1, npts, TY_REAL)
	    call salloc (z1, npts, TY_REAL)
	    call asubkr (x, GS_XREF(sf), Memr[x1], npts)
	    call asubkr (y, GS_YREF(sf), Memr[y1], npts)
	    call asubkr (z, GS_ZREF(sf), Memr[z1], npts)
	}

	# calculate the non-zero basis functions
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE:
	    if (refsub) {
		call rgs_bleg (Memr[x1], npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bleg (Memr[y1], npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    } else {
		call rgs_bleg (x, npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bleg (y, npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    }
	case GS_CHEBYSHEV:
	    if (refsub) {
		call rgs_bcheb (Memr[x1], npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bcheb (Memr[y1], npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    } else {
		call rgs_bcheb (x, npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bcheb (y, npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    }
	case GS_POLYNOMIAL:
	    if (refsub) {
		call rgs_bpol (Memr[x1], npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bpol (Memr[y1], npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    } else {
		call rgs_bpol (x, npts, GS_XORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), XBASIS(GS_XBASIS(sf)))
		call rgs_bpol (y, npts, GS_YORDER(sf), GS_YMAXMIN(sf),
		    GS_YRANGE(sf), YBASIS(GS_YBASIS(sf)))
	    }
	default:
	    call error (0, "GSACCUM: Illegal curve type.")
	}

	# allocate temporary storage space for matrix accumulation
	call salloc (byw, npts, TY_REAL)
	call salloc (bw, npts, TY_REAL)

	# one index the pointers
	vzptr = GS_VECTOR(sf) - 1
	mzptr = GS_MATRIX(sf)
	bxptr = GS_XBASIS(sf)
	byptr = GS_YBASIS(sf)

	switch (GS_TYPE(sf)) {

	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:

	    maxorder = max (GS_XORDER(sf) + 1, GS_YORDER(sf) + 1)
	    xorder = GS_XORDER(sf)
	    ntimes = 0

	    do l = 1, GS_YORDER(sf) {

		call amulr (w, YBASIS(byptr), Memr[byw], npts)
	        bxptr = GS_XBASIS(sf)
	        do k = 1, xorder {
	            call amulr (Memr[byw], XBASIS(bxptr), Memr[bw], npts) 
		    vindex = vzptr + k
	            VECTOR(vindex) = VECTOR(vindex) + adotr (Memr[bw], z,
		        npts)
		    bbyptr = byptr
	            bbxptr = bxptr
		    xxorder = xorder
		    jj = k
		    ll = l
	            ii = 0
		    do j = k + ntimes, GS_NCOEFF(sf) {
		        mindex = mzptr + ii
		        do i = 1, npts
		            MATRIX(mindex) = MATRIX(mindex) + Memr[bw+i-1] *
				XBASIS(bbxptr+i-1) * YBASIS(bbyptr+i-1)	
			if (mod (jj, xxorder) == 0) {
			    jj = 1
			    ll = ll + 1
			    bbxptr = GS_XBASIS(sf)
			    bbyptr = bbyptr + npts
			    switch (GS_XTERMS(sf)) {
			    case GS_XNONE:
				xxorder = 1
			    case GS_XHALF:
				if ((ll + GS_XORDER(sf)) > maxorder)
		    		    xxorder = xxorder - 1
			    default:
				;
			    }
			} else {
			    jj = jj + 1
			    bbxptr = bbxptr + npts
			}
		        ii = ii + 1
		    }
		    mzptr = mzptr + GS_NCOEFF(sf)
		    bxptr = bxptr + npts
	        }

		vzptr = vzptr + xorder
		ntimes = ntimes + xorder
		switch (GS_XTERMS(sf)) {
		case GS_XNONE:
		    xorder = 1
		case GS_XHALF:
		    if ((l + GS_XORDER(sf) + 1) > maxorder)
		        xorder = xorder - 1
		default:
		    ;
		}
		byptr = byptr + npts
	    }

	default:
	    call error (0, "GSACCUM: Unknown curve type.")
	}

	# release the space
	call sfree (sp)
	GS_XBASIS(sf) = NULL
	GS_YBASIS(sf) = NULL
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISLACCUM -- Procedure to add points on a line to the data set.
# The inner products of the non-zero x basis functions are stored
# in the SF_XORDER(sf) by SF_NXCOEFF(sf) matrix XMATRIX. The
# main diagonal is stored in the first row of XMATRIX. Successive
# non-zero diagonals are stored in the succeeding rows. This method
# of storage is particularly efficient for the large symmetric
# banded matrices produced during spline fits. The inner products
# of the data ordinates and the non-zero x basis functions are
# caculated and stored in the lineno-th row of the SF_NXCOEFF(sf)
# by SF_NLINES(sf) matrix XCOEFF.

procedure islaccum (sf, cols, lineno, z, w, ncols, wtflag)

pointer	sf		# pointer to surface descriptor
int	cols[ncols]	# column values
int	lineno		# lineno of data being accumulated
real	z[ncols]	# surface values on lineno at cols
real	w[ncols]	# weight of the data points
int	ncols		# number of data points
int	wtflag		# type of weighting desired

int	i, ii, j, k
pointer	xbzptr, xbptr
pointer	xlzptr
pointer	xmzptr, xmindex
pointer	xczptr, xcindex
pointer	bw, rows, left
pointer sp

begin
	# count the number of points
	SF_NXPTS(sf) = SF_NXPTS(sf) + ncols

	# calculate the weights, default is uniform weighting
	switch (wtflag) {
	case SF_UNIFORM:
	    call amovkr (1.0, w, ncols)
	case SF_USER:
	    # do not alter weights
	default:
	    call amovkr (1.0, w, ncols)
	}

	# set up temporary storage
	call smark (sp)
	call salloc (bw, ncols, TY_REAL)
	call salloc (left, ncols, TY_INT)
	call salloc (rows, ncols, TY_INT)

	# set up the pointers
	xbzptr = SF_XBASIS(sf) - 1
	xmzptr = SF_XMATRIX(sf)
	xczptr = SF_XCOEFF(sf) + (lineno - 1) * SF_NXCOEFF(sf) - 1

	# accumulate the line
	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:

	    do i = 1, SF_XORDER(sf) {
		do j = 1, ncols
		    Memr[bw+j-1] = w[j] * XBASIS(xbzptr+cols[j]) 
		xcindex = xczptr + i
		do j = 1, ncols
		    XCOEFF(xcindex) = XCOEFF(xcindex) + Memr[bw+j-1] * z[j]
		xbptr  = xbzptr
		ii = 0
		do k = i, SF_XORDER(sf) {
		    xmindex = xmzptr + ii
		    do j = 1, ncols
			XMATRIX(xmindex) = XMATRIX(xmindex) + Memr[bw+j-1] *
			    XBASIS(xbptr+cols[j])
		    ii = ii + 1
		    xbptr = xbptr + SF_NCOLS(sf)
		}
		xbzptr = xbzptr + SF_NCOLS(sf)
		xmzptr = xmzptr + SF_XORDER(sf)
	    }

	case SF_SPLINE3, SF_SPLINE1:

	    xlzptr = SF_XLEFT(sf) - 1
	    do j = 1, ncols
	        Memi[left+j-1] = XLEFT(xlzptr+cols[j])
	    call amulki (Memi[left], SF_XORDER(sf), Memi[rows], ncols)
	    call aaddki (Memi[rows], SF_XMATRIX(sf), Memi[rows], ncols)
	    call aaddki (Memi[left], xczptr, Memi[left], ncols) 

	    do i = 1, SF_XORDER(sf) {
		do j = 1, ncols {
		    Memr[bw+j-1] = w[j] * XBASIS(xbzptr+cols[j])
		    xcindex = Memi[left+j-1] + i
		    XCOEFF(xcindex) = XCOEFF(xcindex) + Memr[bw+j-1] * z[j]
		}
		xbptr = xbzptr
		ii = 0
		do k = i, SF_XORDER(sf) {
		    do j = 1, ncols {
			xmindex = Memi[rows+j-1] + ii
			XMATRIX(xmindex) = XMATRIX(xmindex) + Memr[bw+j-1] *
			    XBASIS(xbptr+cols[j])
		    }
		    ii = ii + 1
		    xbptr = xbptr + SF_NCOLS(sf)
		}
		xbzptr = xbzptr + SF_NCOLS(sf)
		call aaddki (Memi[rows], SF_XORDER(sf), Memi[rows], ncols)
	    }
	}

	# release space
	call sfree (sp)
end

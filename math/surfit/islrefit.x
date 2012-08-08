# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISLREFIT -- Procedure to refit the data assuming that the cols and w
# arrays do not change. SIFLREFIT assumes that the Cholesky factorization
# of XMATRIX is stored in XMATRIX. The inner products of the x basis
# functions and the data ordinates are accumulated into the lineno-th
# row of the SF_NXCOEFF(sf) by SF_NLINES(sf) matrix XCOEFF. The coefficients
# for line number lineno are calculated by forward and back
# substitution and placed in the lineno-th row of XCOEFF replacing the
# original data.

procedure islrefit (sf, cols, lineno, z, w)

pointer	sf		# pointer to surface descriptor
int	cols[ARB]	# columns to be fit 
int	lineno		# line number
real	z[ARB]		# surface values
real	w[ARB]		# weight values

int	i, j
pointer	xbzptr, xczptr, xcindex, xlzptr

begin
	# set pointers
	xbzptr = SF_XBASIS(sf) - 1
	xczptr = SF_XCOEFF(sf) + (lineno - 1) * SF_NXCOEFF(sf) - 1
	xlzptr = SF_XLEFT(sf) - 1

	# reset lineno-th row of the x coefficient matrix
	call aclrr (XCOEFF(xczptr+1), SF_NXCOEFF(sf))

	if (SF_WZ(sf) == NULL)
	    call malloc (SF_WZ(sf), SF_NXPTS(sf), MEM_TYPE)

	# calculate new right sides
	call amulr (w, z, Memr[SF_WZ(sf)], SF_NXPTS(sf))

	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:

	    do i = 1, SF_XORDER(sf) {
		xcindex = xczptr + i
		do j = 1, SF_NXPTS(sf)
		    XCOEFF(xcindex) = XCOEFF(xcindex) + Memr[SF_WZ(sf)+j-1] *
			XBASIS(xbzptr+cols[j])
		xbzptr = xbzptr + SF_NCOLS(sf)
	    }

	case SF_SPLINE3, SF_SPLINE1:

	    if (SF_TLEFT(sf) == NULL)
	        call malloc (SF_TLEFT(sf), SF_NXPTS(sf), TY_INT)

	    do i = 1, SF_NXPTS(sf)
		Memi[SF_TLEFT(sf)+i-1] = XLEFT(xlzptr+cols[i]) + xczptr

	    do i = 1, SF_XORDER(sf) {
		do j = 1, SF_NXPTS(sf) {
		    xcindex = Memi[SF_TLEFT(sf)+j-1] + i
		    XCOEFF(xcindex) = XCOEFF(xcindex) + Memr[SF_WZ(sf)+j-1] *
			XBASIS(xbzptr+cols[j])
		}
		xbzptr = xbzptr + SF_NCOLS(sf)
	    }

	}

	# solve for the new x coefficients for line number lineno
	call sfchoslv (XMATRIX(SF_XMATRIX(sf)), SF_XORDER(sf), SF_NXCOEFF(sf),
	    XCOEFF(xczptr+1), XCOEFF(xczptr+1))
end

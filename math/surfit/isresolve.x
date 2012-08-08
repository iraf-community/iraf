# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISRESOLVE -- Procedure to solve the x coefficient matrix for the surface
# coefficients assuming that the lines array is unchanged since the last
# call to SIFSOLVE. The Cholesky factorization of YMATRIX is assumed to be
# stored in YMATRIX. The inner product of the y basis functions and i-th
# column of XCOEFF containing the i-th x coefficients for each line are
# calculated and stored in the i-th row of the SF_NYCOEFF(sf) by
# SF_NXCOEFF(sf) array COEFF. Each of the SF_NXCOEFF(sf) rows of COEFF
# is solved to determine the SF_NYCOEFF(sf) by SF_NXCOEFF(sf) surface
# coefficients. After a call to SIFSOLVE the coefficient for the i-th
# y basis function and the j-th x coefficient is found in the j-th row
# and i-th column of COEFF.

procedure isresolve (sf, lines, ier)

pointer	sf		# pointer to the surface descriptor structure
int	lines[ARB]	# line numbers included in the fit
int	ier		# error code

int	i, j, k, nxcoeff
pointer	ybzptr
pointer	ylzptr
pointer	xczptr, xcptr, xcindex
pointer	czptr, cptr
pointer	left, tleft
pointer	sp

begin
	# define pointers
	ybzptr = SF_YBASIS(sf) - 1
	xczptr = SF_XCOEFF(sf) - SF_NXCOEFF(sf) - 1
	czptr = SF_COEFF(sf) - 1

	# zero out coefficient matrix
	call aclrr (COEFF(SF_COEFF(sf)), SF_NXCOEFF(sf) * SF_NYCOEFF(sf))

	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:

	    # loop over the y basis functions
	    nxcoeff = SF_NXCOEFF(sf)
	    do i = 1, SF_YORDER(sf) {
		cptr = czptr + i
		do k = 1, nxcoeff {
		    xcptr = xczptr + k
		    do j = 1, SF_NYPTS(sf) {
			xcindex = xcptr + lines[j] * SF_NXCOEFF(sf)
		        COEFF(cptr) = COEFF(cptr) +
		    		  YBASIS(ybzptr+lines[j]) * XCOEFF(xcindex)
		    }
		    cptr = cptr + SF_NYCOEFF(sf)
		}

		ybzptr = ybzptr + SF_NYPTS(sf)

		# if SF_XTERMS(sf) = NO do not accumulate elements
		# of COEFF where i != 1 and k != 1
		if (SF_XTERMS(sf) == NO)
		    nxcoeff = 1
	    }

	case SF_SPLINE3, SF_SPLINE1:
	    call smark (sp)
	    call salloc (left, SF_NYPTS(sf), TY_INT)
	    call salloc (tleft, SF_NYPTS(sf), TY_INT)

	    ylzptr = SF_YLEFT(sf) - 1
	    do j = 1, SF_NYPTS(sf)
		Memi[left+j-1] = YLEFT(ylzptr+lines[j])
	    call aaddki (Memi[left], czptr, Memi[left], SF_NYPTS(sf))

	    nxcoeff = SF_NXCOEFF(sf)
	    do i = 1, SF_YORDER(sf) {
		call aaddki (Memi[left], i, Memi[tleft], SF_NYPTS(sf))
		do k = 1, nxcoeff {
		    xcptr = xczptr + k
		    do j = 1, SF_NYPTS(sf) {
			cptr = Memi[tleft+j-1]
			xcindex = xcptr + lines[j] * SF_NXCOEFF(sf)
			COEFF(cptr) = COEFF(cptr) + YBASIS(ybzptr+lines[j]) *
			    XCOEFF(xcindex)
		    }
		    call aaddki (Memi[tleft], SF_NYCOEFF(sf), Memi[tleft],
			SF_NYPTS(sf))
		}

		ybzptr = ybzptr + SF_NYPTS(sf)
	    }

	    call sfree (sp)
	}

	# return if not enough points
	ier = OK
	if ((SF_NYPTS(sf) - SF_NYCOEFF(sf)) < 0) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	if (SF_XTERMS(sf) == YES) {

	    # solve for the nxcoeff right sides
	    cptr = SF_COEFF(sf)
	    do i = 1, SF_NXCOEFF(sf) {
	        call sfchoslv (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf),
	    		       SF_NYCOEFF(sf), COEFF(cptr), COEFF(cptr))
	        cptr = cptr + SF_NYCOEFF(sf)
	    }

	} else {

	    # solve for the y coefficients
	    cptr = SF_NYCOEFF(sf)
	    call sfchoslv (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf),
			      SF_NYCOEFF(sf), COEFF(cptr), COEFF(cptr))

	    # solve for the x coefficients
	    do i = 2, SF_NXCOEFF(sf) {
		cptr = czptr + SF_NYCOEFF(sf)
		COEFF(cptr) = COEFF(cptr) / SF_NYPTS(sf)
	    }
	}
end

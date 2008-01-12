# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISSOLVE -- Procedure to solve the x coefficient matrix for the surface
# coefficients. The inner products of the y basis functions are accumulated
# and stored in the SF_YORDER(sf) by SF_NYCOEFF(sf) array YMATRIX. The
# main diagonal of YMATRIX is stored in the first row of YMATRIX followed
# by the remaining non-zero lower diagonals. The Cholesky factorization
# of YMATRIX is calculated and stored on top of YMATRIX destroying the
# original data. The inner products of the y basis functions and 
# and the i-th column of the SF_NXCOEFF(sf) by SF_NLINES(sf) matrix XCOEFF
# containing the i-th x coefficients for each line are calculated and
# placed in the i-th row of the SF_NYCOEFF(sf) by SF_NXCOEFF(sf) matrix
# COEFF. Each of the SF_NXCOEFF(sf) rows of COEFF is solved to determine
# the SF_NXCOEFF(sf) by SF_NYCOEFF(sf) surface coefficients. After a
# call to SIFSOLVE the coefficient of the i-th x basis function and the
# j-th y basis function will be found in the j-th column and i-th row
# of COEFF.

procedure issolve (sf, lines, nlines, ier)

pointer	sf		# pointer to the curve descriptor structure
int	lines[ARB]	# line numbers included in the fit
int	nlines		# number of lines fit
int	ier		# error code

int	i, ii, j, k, nxcoeff
pointer	ybzptr, ybptr
pointer	ylzptr
pointer	ymzptr, ymindex
pointer	xczptr, xcptr, xcindex
pointer	czptr, cptr
pointer	left, tleft, rows
pointer	sp

begin
	# define pointers
	ybzptr = SF_YBASIS(sf) - 1
	ymzptr = SF_YMATRIX(sf)
	xczptr = SF_XCOEFF(sf) - SF_NXCOEFF(sf) - 1
	czptr = SF_COEFF(sf) - 1

	# zero out coefficient matrix and the y coefficient matrix
	call aclrr (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf) * SF_NYCOEFF(sf))
	call aclrr (COEFF(SF_COEFF(sf)), SF_NXCOEFF(sf) * SF_NYCOEFF(sf))

	# increment the number of points
	SF_NYPTS(sf) =  nlines

	switch (SF_TYPE(sf)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:

	    # accumulate the y value in the y matrix 
	    nxcoeff = SF_NXCOEFF(sf)
	    do i = 1, SF_YORDER(sf) {
		cptr = czptr + i
		do k = 1, nxcoeff {
		    xcptr = xczptr + k
		    do j = 1, nlines {
		        xcindex = xcptr + lines[j] * SF_NXCOEFF(sf)
		        COEFF(cptr) = COEFF(cptr) +
			    YBASIS(ybzptr+lines[j]) * XCOEFF(xcindex)
		    }
		    cptr = cptr + SF_NYCOEFF(sf)
		}
		ii = 0
		ybptr = ybzptr
		do k = i, SF_YORDER(sf) {
		    ymindex = ymzptr + ii
		    do j = 1, nlines  
		        YMATRIX(ymindex) = YMATRIX(ymindex) +
				     YBASIS(ybzptr+lines[j]) *
				     YBASIS(ybptr+lines[j])
		    ii = ii + 1
		    ybptr = ybptr + SF_NLINES(sf)
		}

		if (SF_XTERMS(sf) == NO)
		    nxcoeff = 1

		ybzptr = ybzptr + SF_NLINES(sf)
		ymzptr = ymzptr + SF_YORDER(sf)
	    }

	case SF_SPLINE3, SF_SPLINE1:

	    call smark (sp)
	    call salloc (left, nlines, TY_INT)
	    call salloc (tleft, nlines, TY_INT)
	    call salloc (rows, nlines, TY_INT)

	    ylzptr = SF_YLEFT(sf) - 1
	    do j = 1, nlines
		Memi[left+j-1] = YLEFT(ylzptr+lines[j])
	    call amulki (Memi[left], SF_YORDER(sf), Memi[rows], nlines)
	    call aaddki (Memi[rows], SF_YMATRIX(sf), Memi[rows], nlines)
	    call aaddki (Memi[left], czptr, Memi[left], nlines)

	    # accumulate the y value in the y matrix 
	    nxcoeff = SF_NXCOEFF(sf)
	    do i = 1, SF_YORDER(sf) {
		call aaddki (Memi[left], i, Memi[tleft], nlines)
		do k = 1, nxcoeff {
		    xcptr = xczptr + k
		    do j = 1, nlines {
			cptr = Memi[tleft+j-1]
			xcindex = xcptr + lines[j] * SF_NXCOEFF(sf)
		        COEFF(cptr) = COEFF(cptr) + YBASIS(ybzptr+lines[j]) *
			    XCOEFF(xcindex)
		    }
		    call aaddki (Memi[tleft], SF_NYCOEFF(sf), Memi[tleft],
		        nlines)
		}
		ii = 0
		ybptr = ybzptr
		do k = i, SF_YORDER(sf) {
		    do j = 1, nlines {  
		        ymindex = Memi[rows+j-1] + ii
		        YMATRIX(ymindex) = YMATRIX(ymindex) +
				     YBASIS(ybzptr+lines[j]) *
				     YBASIS(ybptr+lines[j])
		    }
		    ii = ii + 1
		    ybptr = ybptr + SF_NLINES(sf)
		}

		ybzptr = ybzptr + SF_NLINES(sf)
		call aaddki (Memi[rows], SF_YORDER(sf), Memi[rows], nlines)
	    }

	    call sfree (sp)

	}

	# return if not enough points
	ier = OK
	if ((SF_NYPTS(sf) - SF_NYCOEFF(sf)) < 0) {
	    ier = NO_DEG_FREEDOM
	    return
	}

	# calculate the Cholesky factorization of the y matrix
	call sfchofac (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf), SF_NYCOEFF(sf),
		       YMATRIX(SF_YMATRIX(sf)), ier)

	if (SF_XTERMS(sf) == YES) {

	    # solve for the nxcoeff right sides
	    cptr = SF_COEFF(sf)
	    do i = 1, SF_NXCOEFF(sf) {
	        call sfchoslv (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf),
	    		       SF_NYCOEFF(sf), COEFF(cptr), COEFF(cptr))
		cptr = cptr + SF_NYCOEFF(sf)
	    }

	} else {

	    cptr = SF_COEFF(sf)
	    call sfchoslv (YMATRIX(SF_YMATRIX(sf)), SF_YORDER(sf),
			      SF_NYCOEFF(sf), COEFF(cptr), COEFF(cptr))

	    do i = 2, SF_NXCOEFF(sf) {
		cptr = cptr + SF_NYCOEFF(sf)
		COEFF(cptr) = COEFF(cptr) / SF_NYPTS(sf)
	    }
	}
end

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "surfitdef.h"

# ISCOEFF -- Procedure to fetch the number and magnitude of the coefficients.
# If the surface type is a bicubic spline or polynomials with cross terms
# then the  number of coefficients is SF_NXCOEFF(sf) * SF_NYCOEFF(sf) and
# the coefficient of B(i,x) * B(j,y) will be stored in element
# (i - 1) * SF_NYCOEFF(sf) + j of the array coeff. Otherwise the number
# of coefficients will be (SF_NXCOEFF(sf) + SF_NYCOEFF(sf) - 1), and
# the SF_NYCOEFF(sf) y coefficients will be output first followed by
# the (SF_NXCOEFF(sf) - 1) x coefficients.

procedure iscoeff (sf, coeff, ncoeff)

pointer	sf		# pointer to the surface fitting descriptor
real	coeff[ARB]	# the coefficients of the fit
int	ncoeff		# the number of coefficients

int	i
pointer	cptr

begin
	# calculate the number of coefficients
	if (SF_XTERMS(sf) == NO) {
	    ncoeff = SF_NXCOEFF(sf) + SF_NYCOEFF(sf) - 1
	    call amovr (COEFF(SF_COEFF(sf)), coeff, SF_NYCOEFF(sf))
	    cptr = SF_COEFF(sf) + SF_NYCOEFF(sf)
	    do i = SF_NYCOEFF(sf) + 1, ncoeff {
		coeff[i] = COEFF(cptr)
		cptr = cptr + SF_NYCOEFF(sf)
	    }
	} else {
	    ncoeff = SF_NXCOEFF(sf) * SF_NYCOEFF(sf)
	    call amovr (COEFF(SF_COEFF(sf)), coeff, ncoeff)
	}
end

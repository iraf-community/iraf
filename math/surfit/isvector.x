# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISVECTOR -- Procedure to evaluate the fitted surface at an array of points.
# The SF_NXCOEFF(sf) by SF_NYCOEFF(sf) coefficients are stored in the
# SF_NYCOEFF(sf) by SF_NXCOEFF(sf) matrix COEFF. The j-th element of the ith
# row of COEFF contains the coefficient of the i-th basis function in x and
# the j-th basis function in y.

procedure isvector (sf, x, y, zfit, npts)

pointer	sf		# pointer to surface descriptor structure
real	x[ARB]		# x value
real	y[ARB]		# y value
real	zfit[ARB]	# fits surface values
int	npts		# number of data points

int	i
pointer	xcoeff, cptr, sp

begin
	# evaluate the surface along the vector
	switch (SF_TYPE(sf)) {
	case SF_CHEBYSHEV:
	    if (SF_XORDER(sf) == 1) {
		call cv_evcheb (COEFF(SF_COEFF(sf)), y, zfit, npts,
		    SF_YORDER(sf), SF_YMAXMIN(sf), SF_YRANGE(sf))
	    } else if (SF_YORDER(sf) == 1) {
		call smark (sp)
		call salloc (xcoeff, SF_NXCOEFF(sf), MEM_TYPE)
		cptr = SF_COEFF(sf)
		do i = 1, SF_NXCOEFF(sf) {
		    Memr[xcoeff+i-1] = COEFF(cptr)
		    cptr = cptr + SF_NYCOEFF(sf)
		}
		call cv_evcheb (Memr[xcoeff], x, zfit, npts,
		    SF_XORDER(sf), SF_XMAXMIN(sf), SF_XRANGE(sf))
		call sfree (sp)
	    } else
	        call sf_evcheb (COEFF(SF_COEFF(sf)), x, y, zfit, npts,
	            SF_XTERMS(sf), SF_XORDER(sf), SF_YORDER(sf), SF_XMAXMIN(sf),
		    SF_XRANGE(sf), SF_YMAXMIN(sf), SF_YRANGE(sf))

	case SF_LEGENDRE:
	    if (SF_XORDER(sf) == 1) {
		call cv_evleg (COEFF(SF_COEFF(sf)), y, zfit, npts,
		    SF_YORDER(sf), SF_YMAXMIN(sf), SF_YRANGE(sf))
	    } else if (SF_YORDER(sf) == 1) {
		call smark (sp)
		call salloc (xcoeff, SF_NXCOEFF(sf), MEM_TYPE)
		cptr = SF_COEFF(sf)
		do i = 1, SF_NXCOEFF(sf) {
		    Memr[xcoeff+i-1] = COEFF(cptr)
		    cptr = cptr + SF_NYCOEFF(sf)
		}
		call cv_evcheb (Memr[xcoeff], x, zfit, npts,
		    SF_XORDER(sf), SF_XMAXMIN(sf), SF_XRANGE(sf))
		call sfree (sp)
	    } else
	        call sf_evleg (COEFF(SF_COEFF(sf)), x, y, zfit, npts,
		    SF_XTERMS(sf), SF_XORDER(sf), SF_YORDER(sf), SF_XMAXMIN(sf),
		    SF_XRANGE(sf), SF_YMAXMIN(sf), SF_YRANGE(sf))

	case SF_SPLINE3:
	    call sf_evspline3 (COEFF(SF_COEFF(sf)), x, y, zfit, npts,
	        SF_NXPIECES(sf), SF_NYPIECES(sf), -SF_XMIN(sf), SF_XSPACING(sf),
		-SF_YMIN(sf), SF_YSPACING(sf))

	case SF_SPLINE1:
	    call sf_evspline1 (COEFF(SF_COEFF(sf)), x, y, zfit, npts,
	        SF_NXPIECES(sf), SF_NYPIECES(sf), -SF_XMIN(sf), SF_XSPACING(sf),
		-SF_YMIN(sf), SF_YSPACING(sf))
	}
end

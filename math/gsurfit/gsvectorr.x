# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "gsurfitdef.h"

# GSVECTOR -- Procedure to evaluate the fitted surface at an array of points.
# The GS_NCOEFF(sf) coefficients are stored in the
# vector COEFF.

procedure gsvector (sf, x, y, zfit, npts)

pointer	sf		# pointer to surface descriptor structure
real	x[ARB]		# x value
real	y[ARB]		# y value
real	zfit[ARB]	# fits surface values
int	npts		# number of data points

begin
	# evaluate the surface along the vector
	switch (GS_TYPE(sf)) {
	case GS_POLYNOMIAL:
	    if (GS_XORDER(sf) == 1) {
		call rgs_1devpoly (COEFF(GS_COEFF(sf)), y, zfit, npts,
		    GS_YORDER(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	    } else if (GS_YORDER(sf) == 1) {
		call rgs_1devpoly (COEFF(GS_COEFF(sf)), x, zfit, npts,
		    GS_XORDER(sf), GS_XMAXMIN(sf), GS_XRANGE(sf))
	    } else
	        call rgs_evpoly (COEFF(GS_COEFF(sf)), x, y, zfit, npts,
	            GS_XTERMS(sf), GS_XORDER(sf), GS_YORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	case GS_CHEBYSHEV:
	    if (GS_XORDER(sf) == 1) {
		call rgs_1devcheb (COEFF(GS_COEFF(sf)), y, zfit, npts,
		    GS_YORDER(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	    } else if (GS_YORDER(sf) == 1) {
		call rgs_1devcheb (COEFF(GS_COEFF(sf)), x, zfit, npts,
		    GS_XORDER(sf), GS_XMAXMIN(sf), GS_XRANGE(sf))
	    } else
	        call rgs_evcheb (COEFF(GS_COEFF(sf)), x, y, zfit, npts,
	            GS_XTERMS(sf), GS_XORDER(sf), GS_YORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	case GS_LEGENDRE:
	    if (GS_XORDER(sf) == 1) {
		call rgs_1devleg (COEFF(GS_COEFF(sf)), y, zfit, npts,
		    GS_YORDER(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	    } else if (GS_YORDER(sf) == 1) {
		call rgs_1devleg (COEFF(GS_COEFF(sf)), x, zfit, npts,
		    GS_XORDER(sf), GS_XMAXMIN(sf), GS_XRANGE(sf))
	    } else
	        call rgs_evleg (COEFF(GS_COEFF(sf)), x, y, zfit, npts,
		    GS_XTERMS(sf), GS_XORDER(sf), GS_YORDER(sf), GS_XMAXMIN(sf),
		    GS_XRANGE(sf), GS_YMAXMIN(sf), GS_YRANGE(sf))
	default:
	    call error (0, "GSVECTOR: Unknown surface type.")
	}
end

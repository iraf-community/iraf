# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSEVAL -- Procedure to evaluate the fitted surface at a single point.
# The GS_NCOEFF(sf) coefficients are stored in the vector COEFF.

double procedure dgseval (sf, x, y)

pointer	sf		# pointer to surface descriptor structure
double	x		# x value
double	y		# y value

double	sum, accum
int	i, ii, k, maxorder, xorder
pointer	sp, xb, xzb, yb, yzb, czptr
errchk	smark, salloc, sfree

begin
	call smark (sp)

	# allocate space for the basis functions
	switch (GS_TYPE(sf)) {
	case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:
	    call salloc (xb, GS_NXCOEFF(sf), TY_DOUBLE)
	    call salloc (yb, GS_NYCOEFF(sf), TY_DOUBLE)
	    xzb = xb - 1
	    yzb = yb - 1
	    czptr = GS_COEFF(sf) - 1
	default:
	    call error (0, "GSEVAL: Unknown curve type.")
	}

	# calculate the basis functions
	switch (GS_TYPE(sf)) {
	case GS_CHEBYSHEV:
	    call dgs_b1cheb (x, GS_NXCOEFF(sf), GS_XMAXMIN(sf), GS_XRANGE(sf),
		XBS(xb))
	    call dgs_b1cheb (y, GS_NYCOEFF(sf), GS_YMAXMIN(sf), GS_YRANGE(sf),
		YBS(yb))
	case GS_LEGENDRE:
	    call dgs_b1leg (x, GS_NXCOEFF(sf), GS_XMAXMIN(sf), GS_XRANGE(sf),
		XBS(xb))
	    call dgs_b1leg (y, GS_NYCOEFF(sf), GS_YMAXMIN(sf), GS_YRANGE(sf),
		YBS(yb))
	case GS_POLYNOMIAL:
	    call dgs_b1pol (x, GS_NXCOEFF(sf), GS_XMAXMIN(sf), GS_XRANGE(sf),
		XBS(xb))
	    call dgs_b1pol (y, GS_NYCOEFF(sf), GS_YMAXMIN(sf), GS_YRANGE(sf),
		YBS(yb))
	default:
	    call error (0, "GSEVAL: Unknown surface type.")
	}

	# initialize accumulator
	# basis functions
	sum = 0.

	# loop over y basis functions
	maxorder = max (GS_XORDER(sf) + 1, GS_YORDER(sf) + 1)
	xorder = GS_XORDER(sf)
	ii = 1
	do i = 1, GS_YORDER(sf) {

	    # loop over the x basis functions
	    accum = 0.
	    do k = 1, xorder {
		accum = accum + COEFF(czptr+ii) * XBS(xzb+k) 
		ii = ii + 1
	    }
	    accum = accum * YBS(yzb+i)
	    sum = sum + accum

	    # elements of COEFF where neither k = 1 or i = 1
	    # are not calculated if GS_XTERMS(sf) = NO
	    switch (GS_XTERMS(sf)) {
	    case GS_XNONE:
		xorder = 1
	    case GS_XHALF:
		if ((i + GS_XORDER(sf) + 1) > maxorder)
		    xorder = xorder - 1
	    default:
		;
	    }
	}

	call sfree (sp)

	return (sum)
end
